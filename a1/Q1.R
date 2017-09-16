mydata<- read.csv("deliveries.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

#What was the target set by the team? 
#all matches with match id 577
allmatchid577<-subset(mydata,mydata$match_id==577)
#inning 1 : batting team target
inning1<-subset(allmatchid577,allmatchid577$inning==1)
target<-sum(inning1$total_runs)
print(c("target set",target))

#What was the run rate (number of runs per over) required by Royal Challengersto ensure that they at least drew the match with their opponent? 
#sum of total runs column will give target set by batting team
#number of overs=20
# therefore run rate =target/overs
runrate<-target/20
print(c("Runrate required ",runrate))

#Plot a bar chart indicating the number of runs scored by Royal Challengers Bangalore in each over
#inning 2 : RCB
library(ggplot2)
#subset of inning2
inning2<-subset(allmatchid577,allmatchid577$inning==2)
#all overs
overs<-unique(inning2$over)
bar_chart<-data.frame(NA,NA)
names(bar_chart)<-c("over","runs")

for(o in overs){
  #subset of data for current over
  current_over<-subset(inning2,inning2$over==o)
  totalruns<-sum(current_over$total_runs)
  newrow<-data.frame(o,totalruns)
  names(newrow)<-c("over","runs")
  bar_chart<-rbind(bar_chart,newrow)
  
}
orig_names<-names(bar_chart)
names(bar_chart)
bar_chart<-data.frame(bar_chart[-1,])
names(bar_chart)
names(bar_chart)<-orig_names
#bar_chart has the data of number of runs scored in every over
p<-ggplot(bar_chart,aes(bar_chart$over,bar_chart$runs))+geom_bar(stat = "identity",fill="purple",color="black") + ggtitle("Number of runs per over") + xlab("Overs") +ylab("Runs")
p

