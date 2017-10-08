data <- read.csv(file="got_character_train.csv", header=TRUE, sep=",")
mydata<- as.data.frame(data)

# a)The training dataset  contains  a  large  number  of  missing  values. We  will  use  book mentions, gender,
# if the character has dead relations, number of dead relations, nobility, popularity, marital status, age and information 
# about alive spouse, mother, fatherand heir  for  the  prediction.Impute  the  missing  value  of  age 
# by  its  median  and  fill  in  the remaining missing values with -1
names(mydata)

#calculate age median
age_median<-median(mydata$age,na.rm=TRUE)
rows<-nrow(mydata)

#rows with na values
sapply(mydata, function(x) sum(is.na(x)))

for(col in names(mydata)){
  if(col=="age"){ #if column is age, replace missing by median
    for(i in 1:rows){
      if(is.na(mydata[col][i,])){
        mydata[col][i,]<-age_median
      }
    }
  }
  else{
    for(i in 1:rows){
      if(is.na(mydata[col][i,])){
        mydata[col][i,]<- -1
      }
    }
  }
}
sapply(mydata, function(x) sum(is.na(x)))
#-------------------------------------------------------------------------------------------------------

# b)Sample  class  imbalance  is  sometimes  an  issue  with  logistic  regression.  Clearly,  the training dataset
# has a majorimbalance, as the number of characters alive is greater than the  number  of  characters  dead. 
# Represent  this  imbalance  with  a  help  of  a  pi-  chart. Correct this imbalance by upsampling 
# the records of ‘dead’ characters in the training dataset.Represent the division of records in upsampled data 
# also with a pi-  chart.

#number of 1s and 0s
table(mydata$isAlive)

total_alive<-sum(mydata$isAlive==1)
total_dead<-sum(mydata$isAlive==0)

x<-data.frame("alive",total_alive)
names(x)<-c("A","val")
y<-data.frame("dead",total_dead)
names(y)<-c("A","val")
x<-rbind(x,y)
x #x has number of alive and dead in the data frame


library(plotly)

#pie chart of x
p1<- plot_ly(x, labels = ~A, values = ~val, type = 'pie') %>%
  layout(title = 'Imbalance in alive and dead characters',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p1

fd<-factor(mydata$isAlive)
library(caret)
#upsampling for isAlive column
new_data<-upSample(mydata,fd)
nrow(new_data)
table(new_data$isAlive)


total_alive<-sum(new_data$isAlive==1)
total_dead<-sum(new_data$isAlive==0)
x2<-data.frame("alive",total_alive)
names(x2)<-c("A","val")
y2<-data.frame("dead",total_dead)
names(y2)<-c("A","val")
x2<-rbind(x2,y2)
x2

p2 <- plot_ly(x2, labels = ~A, values = ~val, type = 'pie') %>%
  layout(title = 'After upsampling :Imbalance in alive and dead characters',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p2
#------------------------------------------------------------------------------------------------------------------
#c)How are categorical variables dealt with, in logistic regression? Explain.

# Consider the book mentions attribute. If there are five books, a categorical variable will be a single column
# containing the values 'book1','book2',etc to show which book it is present in. But for logistic regression,
# this is split into five columns, namely book1,book2,...book5. Each column has values 0 or 1 only, where 1 indicates 
# it is present in this particular column's book and 0 indicates it is not.
#------------------------------------------------------------------------------------------------------------------

# d)Implement logistic regression model on the training data. The summary statistics of the model 
# will  clearly  indicate  the  significance  of  each  component.  Extract  only  the 
# significant components from the dataset 

train <- new_data

#model 1 based on all factors
model <- glm(isAlive ~ book1+book2+book3+book4+book5+
                male+boolDeadRelations+numDeadRelations+
               isNoble+popularity+isMarried+age+
               isAliveSpouse+isAliveMother+isAliveFather+
               isAliveHeir+isPopular,family=binomial(link='logit'),data=train)
summary(model)
#AIC: 2586.5

#------------------------------------------------------------------------------------------------------------------

# e)and build another logistic regression model on the new data to predict the death of a 
# character inthe series. The Akaike  Information Criteria is used as an evaluation tool 
# for the model. State which model is better using this value. 

#model 2 based on significant components
model2<- glm(isAlive ~ book1+book2+book4+male+boolDeadRelations
             +popularity+isAliveMother,family=binomial(link='logit'),data=train)
summary(model2)
#AIC: 2583.7  lower AIC hence better model


#------------------------------------------------------------------------------------------------------------------

# f)Use  the  two  models  built  to predict the value of ‘isAlive’ parameter in the test data. 
# Build the confusion matrix for both the predictions.

data_2 <- read.csv(file="got_character_test.csv", header=TRUE, sep=",")
test_data<- as.data.frame(data_2)

#clean test data (fill missing values with -1)
for(col in names(test_data)){
  for(i in 1:rows){
      if(is.na(test_data[col][i,])){
        test_data[col][i,]<- -1
      }
    }
  
}

nrow(test_data)
head(test_data)

p1<-predict(model,test_data,type='response')

#install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(test_data$isAlive, p1)[1]
#optCutOff
confusionMatrix(test_data$isAlive, p1, threshold = optCutOff)

# The columns are actuals, while rows are predicteds.
#     0   1
# 0  17   6
# 1 120 344


p2<-predict(model2,test_data,type='response')
optCutOff2 <- optimalCutoff(test_data$isAlive, p2)[1] 
#optCutOff2
confusionMatrix(test_data$isAlive, p2, threshold = optCutOff2)
#     0   1
# 0  15   6
# 1 122 344
# The columns are actuals, while rows are predicteds.


#------------------------------------------------------------------------------------------------------------------

# g)Plot the ROC curves for the predictions made by the two models and state which is a 
# better model for the given test data. 

library(ROCR)
ROCRpred1 <- prediction(p1, test_data$isAlive)
ROCRperf1 <- performance(ROCRpred1, 'tpr','fpr')
plot(ROCRperf1, colorize = TRUE, text.adj = c(-0.2,1.7))
title('model 1')
pred1 <- prediction(p1, test_data$isAlive)
auc <- performance(pred1, measure = "auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.7139625

ROCRpred2 <- prediction(p2, test_data$isAlive)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))
title('model 2')
pred2 <- prediction(p2, test_data$isAlive)
auc <- performance(pred2, measure = "auc")
auc <- auc@y.values[[1]]
auc
#[1] 0.7175182 higher area under the curve=>better model

