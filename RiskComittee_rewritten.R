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
