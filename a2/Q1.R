library(ggplot2)
library(reshape2)

mydata<- read.csv("pokemon.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)
n<-nrow(mydata)
orig<-melt(mydata["Total"])
original_plot<-ggplot (orig, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + geom_vline(xintercept=0)+geom_hline(yintercept=0)+ggtitle("Original")

#Simple Random Sample which includes 70% of the dataset
simple_random<-mydata[sample(nrow(mydata), 0.7*n), ]
simple_random
nrow(simple_random)
sr<-melt(simple_random["Total"])
simple_random_plot<-ggplot (sr, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5)  + geom_vline(xintercept=0)+geom_hline(yintercept=0)+ggtitle("Simple Random")


#Systematic Sample with k = 3 (starting from the first element, i.e. 1st,4th,7th,...etc)
systematic_sample<-data.frame()
for(i in 1:nrow(mydata)){
  row<-mydata[i,]
  if((i-1)%%3==0){
    systematic_sample<-rbind(systematic_sample,row)
  }
  
}
print(systematic_sample)
sys<-melt(systematic_sample["Total"])
sys_plot<-ggplot (sys, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5)  + geom_vline(xintercept=0)+geom_hline(yintercept=0)+ggtitle("Systematic")
#Stratified Random Sample where the strata are ‘male’ and ‘female’
male<-subset(mydata,mydata$Pr_Male>=0.5)
female<-subset(mydata,mydata$Pr_Male<0.5)
#choosing 50% of data from male and 50% from femaleusing simple random sampling
n<-nrow(male)
m<-nrow(female)
male_sample<-mydata[sample(nrow(male), 0.5*n), ]
male_sample
female_sample<-mydata[sample(nrow(female), 0.5*m), ]
female_sample
#binding both
stratified_sample<-rbind(female_sample,male_sample)
print(stratified_sample)
strat<-melt(stratified_sample["Total"])
strat_plot<-ggplot (strat, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5)  + geom_vline(xintercept=0)+geom_hline(yintercept=0)+ggtitle("Stratified")


#Clustered Sample based on primary type(choose 70% of the types)
all_types<-as.vector(unique(mydata["Type_1"]))
n<-nrow(all_types)
#70% of types
sample_types<-sample(n, 0.7*n)
sample_types
nrow(sample_types)
clustered_sample<-data.frame()
for(t in sample_types){
  type<-all_types[t,]
  type<-toString(type)
  #subset of data for each of the sample types
  data<-subset(mydata,mydata$Type_1==type)
  clustered_sample<-rbind(clustered_sample,data)
}
print(clustered_sample)
cl<-melt(clustered_sample["Total"])
clust_plot<-ggplot (cl, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5)  + geom_vline(xintercept=0)+geom_hline(yintercept=0)+ggtitle("Clustered")

#Multiplot to see difference in variation of column Total
multiplot(original_plot,simple_random_plot,sys_plot,strat_plot,clust_plot,cols=3)




