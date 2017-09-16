#DA4 Q1

mydata<- read.csv("D:/DA/cancer_2015.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

cormat <- cor(data.matrix(mydata),na.rm=TRUE)
#---------------------------------------------------------
# (a) Mean of all available samples for that attribute
mydata<- read.csv("cancer_2015.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

#COLUMNS WITH NA VALUES
columns_with_NA <- colnames(mydata)[apply(is.na(mydata), 2, any)]

mean_mydata<-mydata

for(column in columns_with_NA){
  #FOR EACH COLUMN
  df<-as.data.frame(mean_mydata[column])
  print(column)
  names(df)<-"x"
  #MEAN OF THAT COLUMN
  m<-mean(df$x,na.rm=TRUE)
  # ROWS WITH NA VALUES IN THIS COLUMN
  na_rows<-which(is.na(df), arr.ind=TRUE)
  for(i in 1:nrow(na_rows)){
    # FOR EACH ROW, REPLACING IT BY THE MEAN
    row<-na_rows[i]
    df[row,1]<-m
    print(c(row,m))
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

#FUNCTION THAT FINDS d GIVEN a,b,c and x  FOR THE EQUATION ax^2+bx+c=d
Substitute<-function(A,x){
  t<-A[1]*x*x + A[2]*x + A[3]
  return(t)
}

for(column in columns_with_NA){
  # FOR EACH COLUMN WITH NA VALUES
  df<-as.data.frame(quad_mydata[column])
  names(df)<-"x"
  na_rows<-which(is.na(df), arr.ind=TRUE)
  #PREV IS ROW RIGHT BEFORE IT, BEF_PREV IS BEFORE PREV AND LAST IS THE ROW IMMEDIATELY AFTER THE LAST NA VALUE
  prev<-na_rows[1]-1                      #i
  bef_prev<-prev-1                        #j
  last<-na_rows[nrow(na_rows)]+1          #k
  while(prev<last-1){
    
    prev_d<-df$x[prev]                    #l
    bef_prev_d<-df$x[bef_prev]            #m
    last_d<-df$x[last]                    #n
    A<-c(bef_prev*bef_prev,bef_prev,1,bef_prev_d)  # ai^2+bi+c=l
    B<-c(prev*prev,prev,1,prev_d)                  # aj^2+bj+c=m
    C<-c(last*last,last,1,last_d)                  # ak^2+bk+c=n
    a_b_c<-Solve(A,B,C)                            #solve the above 3 equations
    current<-prev+1
    answer<-Substitute(a_b_c,prev+1)              #substitue for current row
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
plot(density(as.numeric(unlist(mean_mydata[5]))),xlab="Outpatients",main="Comparison of 9 versions of outpatients data produced",col="red")
lines(density(as.numeric(unlist(ldata[5]))),col="blue")
lines(density(as.numeric(unlist(quad_mydata[5]))),col="green")
lines(density(as.numeric(unlist(rdata[5]))),col="yellow")
lines(density(as.numeric(unlist(mice1[5]))),col="orange")
lines(density(as.numeric(unlist(mice2[5]))),col="brown")
lines(density(as.numeric(unlist(mice3[5]))),col="pink")
lines(density(as.numeric(unlist(mice4[5]))),col="purple")
lines(density(as.numeric(unlist(mice5[5]))),col="black")

dev.new()
#plotting density curves for new registrations
plot(density(as.numeric(unlist(mean_mydata[6]))),xlab="New Registrations",main="Comparison of 9 versions of new registrations data produced",col="red")
lines(density(as.numeric(unlist(ldata[6]))),col="blue")
lines(density(as.numeric(unlist(quad_mydata[6]))),col="green")
lines(density(as.numeric(unlist(rdata[6]))),col="yellow")
lines(density(as.numeric(unlist(mice1[6]))),col="orange")
lines(density(as.numeric(unlist(mice2[6]))),col="brown")
lines(density(as.numeric(unlist(mice3[6]))),col="pink")
lines(density(as.numeric(unlist(mice4[6]))),col="purple")
lines(density(as.numeric(unlist(mice5[6]))),col="black")

dev.new()
#plotting density curves for laboratory investigations
plot(density(as.numeric(unlist(mean_mydata[7]))),xlab="Laboratory Investigations",main="Comparison of 9 versions of laboratory investigations data produced",col="red")
lines(density(as.numeric(unlist(ldata[7]))),col="blue")
lines(density(as.numeric(unlist(quad_mydata[7]))),col="green")
lines(density(as.numeric(unlist(rdata[7]))),col="yellow")
lines(density(as.numeric(unlist(mice1[7]))),col="orange")
lines(density(as.numeric(unlist(mice2[7]))),col="brown")
lines(density(as.numeric(unlist(mice3[7]))),col="pink")
lines(density(as.numeric(unlist(mice4[7]))),col="purple")
lines(density(as.numeric(unlist(mice5[7]))),col="black")


