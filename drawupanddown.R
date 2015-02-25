#Monte carlo simulation to rearrange the order of return
#to generate max drawup drawdown

#return a series of max drawups and drawdowns. 
drawupanddown = function(filename, lookbackperiod, NofSimulation){
  require(xts)
  
  #extract out data from file as xts
  dataset = newread.hdd(filename)
  
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
  
  
  #calculate returns
  roc = ROC(dataset$Close,n=1,type="continuous")
  rocforUse = last(roc, paste(timelength,timeunit))
  
  #create empty result matrix
  result = matrix(,nrow = NofSimulation+1,ncol=2)
  temp1 = as.vector(rocforUse$Close)
  
  #Calculate observed drawup and drawdown and restore on the first row of result
  trueDrawUp  = maxSubArraySum(temp1)
  trueDrawDown = minSubArraySum(temp1)
  result[1,1] = trueDrawUp
  result[1,2] = trueDrawDown
  
  
  #Start of MonteCarlo Simulation
  for (i in 1:NofSimulation){
    #generate random permutation 
    index = sample(nrow(rocforUse))
    temp1 = as.vector(rocforUse$Close)
    #combine the column of return with the column of permutation
    temp2 = cbind(temp1,index)
    #sort by the order of the index, ascending
    temp2 = temp2[order(temp2[,2]),] 
    #compute max drawdown and up
    result[i+1,1] = maxSubArraySum(temp2[,1])
    result[i+1,2]= minSubArraySum(temp2[,1])

  }
  
  return (result)
}

ma = drawupanddown("GBPUSD_D1_Bid.csv","3M",1000)

#draw histogram of max drawup and drawdown
hist(ma[,1], main="Histogram of Max DrawUp")
abline(v = ma[1,1], col = "blue")
text(ma[1,1]+0.002,200, "Obs DrawUp", col = "red")


hist(ma[,2], main="Histogram of Max DrawUp")
abline(v = ma[1,2], col = "blue")
text(ma[1,2],200, "Obs DrawDown", col = "red")
