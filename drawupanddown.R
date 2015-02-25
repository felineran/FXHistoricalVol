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
  roc = ROC(dataset$Close,n=1,type="continuous")
  if (toupper(timeunit)=="D"){
    timeunit<-"days"
  }else if(toupper(timeunit)=="M"){
    timeunit<-"months"
  }else{
    timeunit<-"years"
  }
  
  
  #calculate returns
  rocforUse = last(roc, paste(timelength,timeunit))
  
  #Start of MonteCarlo Simulation
  
  #create empty result matrix
  result = matrix(,nrow = NofSimulation, ncol=2)
  temp1 = as.vector(rocforUse$Close)
 
  for (i in 1:NofSimulation){
    #generate random permutation 
    index = sample(nrow(rocforUse))
    #combine the column of return with the column of permutation
    temp2 = cbind(temp1,index)
    #sort by the order of the index, ascending
    temp2 = temp2[order(temp2[,2]),] 
    #compute max drawdown and up
    result[i,1] = maxSubArraySum(temp2[,1])
    result[i,2]= minSubArraySum(temp2[,1])

  }
  
  return (result)
}

ma = drawupanddown("GBPUSD_D1_Bid.csv","10D",100)