mydata<- read.csv("deliveries.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)
#Draw the boxplot of the total number  of  runs  scored  by  them  in  each  match
players<-c("V Kohli","MS Dhoni")
player1<-subset(mydata,mydata$batsman==players[1])
player2<-subset(mydata,mydata$batsman==players[2])

#Function to find out total runs scored by a batsman in each match he has played
info<-function(dataset){
  result<-data.frame(NA)
  names(result)<-c("total_runs")
  #required columns are match id and total runs
  allmatches<-unique(dataset$match_id)
  for(matchid in allmatches){
    #for every match subset of current match
    current_match<-subset(dataset,dataset$match_id==matchid)
    #total runs in this match
    totalruns<-sum(current_match$total_runs)
    #append to result 
    newrow<-data.frame(totalruns)
    names(newrow)<-c("total_runs")
    result<-rbind(result,newrow)
    
  }
  return(result)
  
}

result1<-info(player1)
orig_names<-names(result1)
result1<-data.frame(result1[-1,])
names(result1)<-orig_names
#90th percentile calculation
perc1<-quantile(result1$total_runs, c(.90))
names(result1)<-c("V Kohli")

result2<-info(player2)
orig_names<-names(result2)
result2<-data.frame(result2[-1,])
names(result2)<-orig_names
#90th percentile calculation
perc2<-quantile(result2$total_runs, c(.90))
names(result2)<-c("MS Dhoni")

library(ggplot2)
require(reshape2)
#Boxplots :
p1<-ggplot(melt(result1), aes(variable, value)) + geom_boxplot(color="black",fill="red")+ ggtitle("V Kohli")
print(p1)
p2<-ggplot(melt(result2), aes(variable, value)) + geom_boxplot(color="black",fill="blue")+ ggtitle("MS Dhoni")
print(p2)

print("90th percentile :")
print(c(perc1,perc2))


         