mydata<- read.csv("deliveries.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

#PLAYERS: AB  de  Villiers ,  MS Dhoni,  RA  Jadeja,  SK  Raina  and V  Kohli
players<-c( "AB de Villiers", "MS Dhoni","RA Jadeja",  "SK Raina", "V Kohli")

#total runs scored, mean, median, mode, 1stquartile, 3rdquartile,IQR and standard  deviation  of runs 
result<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA)
names(result)<-c("batsman","total_runs", "mean", "median", "mode", "Q1","Q3","IQR","stddev" )

total_runs_scored<-function(data1){
  x<-sum(data1$total_runs)
  return(x)
}

for(player in players){
  #subset of player
  dataset<-subset(mydata,mydata$batsman==player)
  #calculate total using user defined function
  total<-total_runs_scored(dataset)
  #Summary of dataset gives mean,median,quartiles,etc
  summ<-summary(dataset$total_runs)
  q1<-summ[2]
  med<-summ[3]
  mean1<-summ[4]
  q3<-summ[5]
  iqr<-q3-q1
  stddev<-sd(dataset$total_runs)
  temp <- table(as.vector(dataset$total_runs))
  mode1<-names(temp)[temp == max(temp)]
  newrow<-data.frame(player,total,mean1,med,mode1,q1,q3,iqr,stddev)
  names(newrow)<-c("batsman","total_runs", "mean", "median", "mode", "Q1","Q3","IQR","stddev" )
  result<-rbind(result,newrow)
  
  
}
orig_names<-names(result)
result<-data.frame(result[-1,])
names(result)<-orig_names
print(result)



