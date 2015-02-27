#read in data from csv file and convert to xts format
newread.hdd = function(filename){
     #load packages
      require(xts)
      require(timeSeries)
      require(quantmod)
      
      #get writing path
      owd = getwd()
      
      #dataset file path
      datasetname = paste0("X:/PropTrading/Bloomberg/Data/",filename)
      #load data
      dataset = read.csv(datasetname)
      #convert string into Datetime, by which defines the sorting order
      ordering = strptime(paste(dataset$Date, dataset$Time),
                           format="%m/%d/%Y %H:%M:%S", tz="GMT")
      datasetxts = xts(dataset[,3:ncol(dataset)], order.by=as.POSIXct(ordering))
      on.exit(setwd(owd))

      return(datasetxts)
      }
      
 #calculate standard divation of returns, given time series data xts and lookback days
xtsstanddev= function(datasetxts, lookbackdays){
      require(xts)
      require(TTR)
      
      #calculate return
	roc = ROC(datasetstx[,4], n=1, type="discrete")
	#take out the part for use according to lookbackdays
      dataforUse =  last(roc, paste(lookback,"days"))
	#calculate standard deviation of returns.
      standdev = sd(dataforUse)
 
      return(standdev)
      }

############### I'll explain in detail why this is not precisely what we're 
############### trying to measure, though the code works to intention.
############### The thing I would change is the maxSum functionality.

#returns the largest positive sum of a subarray of a vector 
# or returns 0 if the whole vector is negative
maxSubArraySum = function(vector){
  sum = 0
  maxSum = -2147483647
  for (i in 1:length(vector)){
    sum = sum+ vector[i]
    
    #find max number in the vector, incase all negative
  
    if(sum<0){
      sum= 0
    }
    
    if (sum>maxSum){
      maxSum = sum
    }
  }
  
  #if no positive maxSum, return 0
  if (maxSum == -2147483647)
    maxSum = 0
  
  return (maxSum)
}

############### I'll explain in detail why this is not precisely what we're 
############### trying to measure, though the code works to intention.
############### The thing I would change is the minSum functionality.

#returns the smallest negative sum of a subarray of a vector 
# or returns 0 if the whole vector is positive
minSubArraySum = function(vector){
  sum = 0
  minSum = 2147483647
  for (i in 1:length(vector)){
    sum=sum+vector[i]
    
    #find max number in the vector, incase all negative
    if(sum>0){
      sum= 0
    }
    
    if (sum<minSum){
      minSum = sum
    }
  }
  
  if (minSum == 2147483647){
    minSum = 0
  }
  return (minSum)
}

#Monte carlo simulation to rearrange the order of return
#to generate max drawup drawdown

#return a series of max drawups and drawdowns. 
drawupanddown = function(filename, lookbackperiod, NofSimulation){
  require(xts)
  
  #extract out data from file as xts
  dataset = newread.hdd(filename)
  
  ############### This works to intenion but I think it's more complication than
  ############### you really need when you can realistically set lookbackperiod
  ############### to a number of days. If you want years, simply do quick 
  ############### multiplication, etc. 
  
  #handle lookbackperiod
  #seperate number and letters
  timeunit = gsub("[[:digit:]]","",lookbackperiod)
  timelength = as.numeric(gsub("[[:alpha:]]","",lookbackperiod))
  #ignore caps
  if (toupper(timeunit)=="D"){
    timeunit<-"days"
  }else if(toupper(timeunit)=="M"){
    timeunit<-"months"
  }else{
    timeunit<-"years"
  }
  
  ############### I've switched this to "discrete" for reasons you'll see below
  
  #calculate returns
  roc = ROC(dataset$Close,n=1,type="discrete")
  
  ############### Instead of using "last", you can use different indexing options
  ############### which I will explain in further detail. This works but adds the
  ############### complication of specifying M, D, etc. 
  
  rocforUse = last(roc, paste(timelength,timeunit))
  
  #create empty result matrix
  result = matrix(,nrow = NofSimulation + 1,ncol=2)
  temp1 = as.vector(rocforUse$Close)
  
  ############### This is the biggest change I made. See comments in Runups()
  ############### for more on this. The PerformanceAnalytics package offers us 
  ############### a function that does exactly what we want it to do. They don't
  ############### have a max run-up function but I've modified the Drawdowns()
  ############### function to calculate this much.
  
  ############### I've made this calculation slighly more complicated than it 
  ############### really needs to be because I want to extend functionality in 
  ############### future builds.
  
  drawdowns = PerformanceAnalytics:::Drawdowns(rocforUse)
  runups    = Runups(rocforUse)
 
  ############### Take the minimum drawdown and maximum runup as our results for
  ############### the observed drawdown and runup
  
  result[1,1] = min(drawdowns[,1])
  result[1,2] = max(runups[,1])
  

  #Start of MonteCarlo Simulation
  for (i in 1:NofSimulation){
    #generate random permutation 
    ############### Good.
    index     = sample(nrow(rocforUse))
    #rearrange the vector according to the index given, ascendingly 
    tempret   = as.vector(rocforUse)[index]
    
    ############### This is more complicated than it needs to be right now but
    ############### I've done this intentionally such that I can extend functionality
    ############### in the future. 
    
    drawdowns = merge(drawdowns,PerformanceAnalytics:::Drawdowns(tempret))
    runups    = merge(runups, Runups(tempret))
    
    ############### Good.
    #compute max drawdown and up 
    
    #Should the index on the RHS be i+1 or i?
    result[i+1,1] = min(drawdowns[,i+1])
    result[i+1,2] = max(runups[,i+1])

  }
  
  #Draw Histogram of the result
  par(mfrow=c(2,1))
  histdata = hist(result[,1], main="Histogram of Max Drawdown", xlab="Drawdowns")
  abline(v = result[1,1], col="red", lty=2)
  text(result[1,1], mean(histdata$counts), "Obs Drawdown", col = "red")
  histdatarunup = hist(result[,2], main="Histogram of Max Runup", xlab="Runups")
  abline(v = result[1,2], col = "red", lty=2)
  text(result[1,2],mean(histdatarunup$counts), "Obs Runup", col = "red")
  
  return (result)
}

############### We'll discuss this further. 

Runups = function(returns, geometric=TRUE){
     
     require(xts)
     require(PerformanceAnalytics)

     ############### This is to convert back to xts. It will give us errors and
     ############### is currently a bit sloppy when using a vector as input. I
     ############### will have you fix this. 
     
     #convert vector to xts
     x = checkData(returns)
     columns = ncol(x)
     columnnames = colnames(x)
     
     ############### This is logic we'll discuss further.
     
     colRunup = function(x, geometric) {
        if (geometric) 
            #if return is geometric, compute price level conpoundly
            Return.cumulative = cumprod(1 + x)
        else Return.cumulative = 1 + cumsum(x)
        #vector[-1] cut out the first elt
        #cummin return min value up to current index
        minCumulativeReturn = cummin(c(1, Return.cumulative))[-1]
        column.runup = Return.cumulative/minCumulativeReturn - 1
        return(column.runup)
                                           } 
                                           
     ############### This will allow us to run this function on a multivariate
     ############### array of returns. We're not using it now but may in the
     ############### future. 
     
     
     for (column in 1:columns) {
        #ignore NA data?
        column.runup = PerformanceAnalytics:::na.skip(x[, column], FUN = colRunup, 
            geometric = geometric)
        if (column == 1) 
            runup = column.runup
        else runup = merge(runup, column.runup)
        }
    colnames(runup) = columnnames
    
    ############### Converting the data back to xts. Not using this currently 
    ############### as we're using numeric vectors in lieu of xts class objects.
    
    runup = reclass(runup, x)
    return(runup)
   
    }

#analyze historical intraday Runup and Drawdown
#lookbackperiod in unit of day
#ifHistogram takes TRUE or FALSE for drawing histogram of the result or not
#percentile takes integer to mark out certain percentile on the histogram
inDayUpnDown = function(filename,lookbackperiod, ifHistogram, percentile){
  require(xts)
  
  #extract out data from file as xts
  dataset = newread.hdd(filename)
  
  roc = ROC(dataset$Close,n=1,type="discrete")
  dataforUse = last(dataset,paste(lookbackperiod,"days"))
  
  #split intraday data according to date
  intraDayList = split(dataforUse, f = "days")
  
  #create empty result matrix
  result =  matrix(,nrow = length(intraDayList), ncol=2)
  
  for (i in 1:length(intraDayList)){
    rocforUse = ROC(intraDayList[[i]]$Close,n=1,type="discrete")
    tempret   = as.vector(rocforUse)
    drawdowns = PerformanceAnalytics:::Drawdowns(tempret[2:length(tempret)])
    runups    = Runups(tempret[2:length(tempret)])
    result[i,1] = min(drawdowns)
    result[i,2] = max(runups)
  }
  
  if(ifHistogram){
    par(mfrow=c(2,1))
    
    histdata = hist(result[,1], main="Histogram of Max Drawdown", xlab="Drawdowns")
    abline(v = quantile(result[,1],percentile/100), col="red", lty=2)
    lab = round(as.numeric(quantile(result[,1],percentile/100)),digits=4)
    text(quantile(result[,1],percentile/100), mean(histdata$counts), paste0(percentile,"th Percentile"), col = "red")
    axis(1,at = lab ,label = lab) #mark x intercept of abline
    
    histdatarunup = hist(result[,2], main="Histogram of Max Runup", xlab="Runups")
    abline(v = quantile(result[,2],percentile/100), col = "red", lty=2)
    text(quantile(result[,2],percentile/100),mean(histdatarunup$counts), paste0(percentile,"th Percentile"), col = "red")
    lab = round(as.numeric(quantile(result[,2],percentile/100)),digits=4)
    axis(1,at = lab ,label = lab)
  }
  
  
  
  return (result)
}

########## It's good practice to keep example usage separate to your source code files.
########## Further, this plot functionality should be built into the functions above.

#ma = drawupanddown(filename="EURUSD_D1_Bid.csv",lookbackperiod="365D",NofSimulation=1000)
result = inDayUpnDown("GBPUSD_m15_Bid.csv",50,TRUE, 95)
#draw histogram of max drawup and drawdown

########## This makes two charts within the same graphical device. 
#par(mfrow=c(2,1))

########## The hist() function returns an object of class "histogram". The data
########## within will be necessary to add anything to this chart. I've assigned
########## the results to variable histdata. I've also added the parameter 
########## xlab="Drawdowns" for more professional appearance. 

#histdata = hist(ma[,1], main="Histogram of Max Drawdown", xlab="Drawdowns")

########## For cosmetic reasons I've changed this to a dashed red line such that
########## it's easier to spot immediately. Blue does not stand out as much, and
########## it's worth noting that dashed lines will be much easier to see on 
########## paper printed in black and white. 

#abline(v = ma[1,1], col="red", lty=2)

########## In your original code you used a fairly arbitrary '150' as the y 
########## value. If you work through a number of different NofSimulations you'll
########## see that this hard-coded value will mean the function won't work properly.
########## I've thus changed the '150' to mean(histdata$counts), which will ensure
########## the text displays regardless of NofSimulations. 

#text(ma[1,1], mean(histdata$counts), "Obs Drawdown", col = "red")


#histdatarunup = hist(ma[,2], main="Histogram of Max Runup", xlab="Runups")
#abline(v = ma[1,2], col = "red", lty=2)
#text(ma[1,2],mean(histdatarunup$counts), "Obs Runup", col = "red")
