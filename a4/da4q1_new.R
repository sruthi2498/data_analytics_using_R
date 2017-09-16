#DA4 Q1

mydata<- read.csv("D:/DA/cancer_2015.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

cormat <- cor(data.matrix(mydata),na.rm=TRUE)
#---------------------------------------------------------
# (a) Mean of all available samples for that attribute

columns_with_NA <- colnames(mydata)[apply(is.na(mydata), 2, any)]

mean_mydata<-mydata

for(column in columns_with_NA){
  df<-as.data.frame(mean_mydata[column])
  names(df)<-"x"
  m<-mean(df$x,na.rm=TRUE)
  na_rows<-which(is.na(df), arr.ind=TRUE)
  for(i in 1:nrow(na_rows)){
    row<-na_rows[i]
    df[row,1]<-m
  }
  mean_mydata[column]<-df
}
mean_mydata


#---------------------------------------------------------

#---------------------------------------------------------
# (b) Linear interpolation

ldata <- mydata
#l <- colnames(ldata)[apply(is.na(ldata),2,any)]
na <- which(is.na(ldata),arr.ind=TRUE)
nrow(na)
i<- 1
while(i<=nrow(na))
{
  j<- na[i,1]
  while(is.na(ldata[j,na[i,2]]))
    j <- j+1
  ldata[na[i,1],na[i,2]]=(ldata[j,na[i,2]]+ldata[na[i,1]-1,na[i,2]])/2
  print(i)
  i <- i+1
}

#---------------------------------------------------------

#---------------------------------------------------------

# (c) Quadratic interpolation selecting the most proximate two values from the past and one from the future

columns_with_NA <- colnames(mydata)[apply(is.na(mydata), 2, any)]
quad_mydata<-mydata


#FUNCTION RETURN TO COMPUTE a,b and c  FOR EQUATIONS OF THE FORM ax^2+bx+c=d
Solve<-function(P,Q,R){
  for(i in 1:length(P)){
    Q[i]<-Q[i]-P[i]
  }
  for(i in 1:length(P)){
    R[i]<-R[i]-P[i]
  }
  q<-Q[1]
  r<-R[1]
  for(i in 1:length(P)){
    Q[i]<- (Q[i]/q)*r
  }
  for(i in 1:length(Q)){
    Q[i]<-Q[i]-R[i]
  }
  b<-Q[4]/Q[2]
  a<-(R[4]- (R[2]*b))/R[1]
  c<-(P[4]- (P[2]*b) - (P[1]*a))/P[3]
  ans<-c(a,b,c)
  return(ans)
  
}

Substitute<-function(A,x){
  t<-A[1]*x*x + A[2]*x + A[3]
  return(t)
}

for(column in columns_with_NA){
  df<-as.data.frame(quad_mydata[column])
  names(df)<-"x"
  na_rows<-which(is.na(df), arr.ind=TRUE)
  #print(na_rows)
  prev<-na_rows[1]-1
  bef_prev<-prev-1
  last<-na_rows[nrow(na_rows)]+1
  while(prev<last-1){
    prev_d<-df$x[prev]
    bef_prev_d<-df$x[bef_prev]
    last_d<-df$x[last]
    A<-c(bef_prev*bef_prev,bef_prev,1,bef_prev_d)
    B<-c(prev*prev,prev,1,prev_d)
    C<-c(last*last,last,1,last_d)
    a_b_c<-Solve(A,B,C)
    current<-prev+1
    answer<-Substitute(a_b_c,prev+1)
    #print(answer)
    df[current,1]<-answer
    bef_prev<-prev
    prev<-prev+1
    
  }
  quad_mydata[column]<-df
  
}
#---------------------------------------------------------
#---------------------------------------------------------
# (d) Linear regression

rdata <- mydata
na <- which(is.na(rdata),arr.ind=TRUE)
i<-1
while(i<=nrow(na))
{
y <- as.numeric(unlist(rdata[na[i,2]]))
x <- as.numeric(unlist(rdata[1]))
relation <- lm(y~x)
a <- data.frame(x = x[na[i,1]])
rdata[na[i,1],na[i,2]] <-  predict(relation,a)
i<- i + 1
}
#---------------------------------------------------------
#---------------------------------------------------------
# (e) MICE
#install.packages("mice")
library(mice)
miceData <- mydata
imputed_Data <- mice(miceData, m=5, maxit = 50, method = 'pmm', seed = 500)

mice1 <- complete(imputed_Data,1)
mice2 <- complete(imputed_Data,2)
mice3 <- complete(imputed_Data,3)
mice4 <- complete(imputed_Data,4)
mice5 <- complete(imputed_Data,5)

dev.new()
#plotting density curves for outpatients
plot(density(as.numeric(unlist(mice5[5]))),xlab="Outpatients",main="Comparison of 9 versions of outpatients data produced",col="black")
lines(density(as.numeric(unlist(ldata[5]))),col="blue")
lines(density(as.numeric(unlist(quad_mydata[5]))),col="green")
lines(density(as.numeric(unlist(rdata[5]))),col="yellow")
lines(density(as.numeric(unlist(mice1[5]))),col="black")
lines(density(as.numeric(unlist(mice2[5]))),col="black")
lines(density(as.numeric(unlist(mice3[5]))),col="black")
lines(density(as.numeric(unlist(mice4[5]))),col="black")
lines(density(as.numeric(unlist(mean_mydata[5]))),col="red")
legend(10, 95, legend=c("mean", "Lin Inter","Quad Inter","Linear Reg","Mice1","Mice2","Mice3","Mice4","Mice5"),
       col=c("red", "blue","green","yellow","orange","brown","pink","purple","black"),text.font=4, bg='lightblue')

dev.new()
#plotting density curves for new registrations
plot(density(as.numeric(unlist(mean_mydata[6]))),xlab="New Registrations",main="Comparison of 9 versions of new registrations data produced",col="red")
lines(density(as.numeric(unlist(ldata[6]))),col="blue")
lines(density(as.numeric(unlist(quad_mydata[6]))),col="green")
lines(density(as.numeric(unlist(rdata[6]))),col="yellow")
lines(density(as.numeric(unlist(mice1[6]))),col="black")
lines(density(as.numeric(unlist(mice2[6]))),col="black")
lines(density(as.numeric(unlist(mice3[6]))),col="black")
lines(density(as.numeric(unlist(mice4[6]))),col="black")
lines(density(as.numeric(unlist(mice5[6]))),col="black")

dev.new()
#plotting density curves for laboratory investigations
plot(density(as.numeric(unlist(mice5[7]))),xlab="Laboratory Investigations",main="Comparison of 9 versions of laboratory investigations data produced",col="black")
lines(density(as.numeric(unlist(ldata[7]))),col="blue")
lines(density(as.numeric(unlist(quad_mydata[7]))),col="green")
lines(density(as.numeric(unlist(rdata[7]))),col="yellow")
lines(density(as.numeric(unlist(mice1[7]))),col="black")
lines(density(as.numeric(unlist(mice2[7]))),col="black")
lines(density(as.numeric(unlist(mice3[7]))),col="black")
lines(density(as.numeric(unlist(mice4[7]))),col="black")
lines(density(as.numeric(unlist(mean_mydata[7]))),col="red")


