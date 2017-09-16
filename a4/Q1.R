
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

# (c) Quadratic interpolation selecting the most proximate two values from the past and one from the future

mydata<- read.csv("cancer_2015.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

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
    df[current,1]<-answer
    bef_prev<-prev
    prev<-prev+1
    
  }
  quad_mydata[column]<-df
  
}
quad_mydata




