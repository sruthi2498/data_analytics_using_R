mydata<- read.csv("insertion_time.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)
# Find the statistics.
# a) max time for insertion
# b) max time for deletion
# c) average of all operations
# d) average of insertions
# e) average of deletions.

result<-data.frame()
increase_factors<-unique(mydata$increase_factor)
for(i in increase_factors){
  data<-subset(mydata,mydata$increase_factor==i)
  m<-max(data$time)
  avg<-mean(data$time)
  x<-data.frame(i,m,avg)
  result<-rbind(result,x)
}
names(result)<-c("increase_factor","max_time","avg_time")
print(result)

mydata<- read.csv("deletion_time.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

result2<-data.frame()
decrease_factors<-unique(mydata$decrease_factor)
decrease_factors
for(i in decrease_factors){
  data<-subset(mydata,mydata$decrease_factor==i)
  m<-max(data$time)
  avg<-mean(data$time)
  x<-data.frame(i,m,avg)
  result2<-rbind(result2,x)
}
names(result2)<-c("decrease_factor","max_time","avg_time")
print(result2)

mydata<- read.csv("id.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

result3<-data.frame()
factors<-unique(mydata$increase_factor)
factors
for(i in factors){
  data<-subset(mydata,mydata$increase_factor==i)
  m<-max(data$total_time)
  avg<-mean(data$total_time)
  d<-data$decrease_factor[1]
  x<-data.frame(i,d,m,avg)
  result3<-rbind(result3,x)
}
names(result3)<-c("increase_factor","decrease_factor","max_time","avg_time")
print(result3)




