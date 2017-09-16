#install.packages("caret")
#install.packages("e1071")
mydata<- read.csv("cancer_tailored.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)
library(caret)

M<-confusionMatrix(mydata$predicted,mydata$diagnosis,positive = "M")

TN<-paste("True Negative",table[1,1],sep=" ")
FN<-paste("False Negative",table[2,1],sep=" ")
TP<-paste("True Positive",table[2,2],sep=" ")
FP<-paste("False Positive",table[1,2],sep=" ")

M$table[1,1]<-TN
M$table[2,1]<-FN
M$table[2,2]<-TP
M$table[1,2]<-FN

M$table

#(i)Accuracy
print(M$overall[1])

#(ii)Misclassification Rate (1-accuracy)
print(1-M$overall[1])

#(iii)Recall
print(M$byClass[6])

#(iv)Precision
print(M$byClass[5])

#(v)Specificity
print(M$byClass[2])

#(vi)F Score with β = 2 and 0.5(also state what the corresponding values of β indicate)

#the F1 score (also F-score or F-measure) is a measure of a test's accuracy. 
# F= ( (1+beta^2) * precision * recall )/( (beta^2 * precision) + recall)

precision<-M$byClass[5]
names(precision)<-NULL
recall<-M$byClass[6]
names(recall)<-NULL

F_2<-( (1+4) * precision * recall )/( (4 * precision) + recall)
F_2

F_0.5<-( (1+0.25) * precision * recall )/( (0.25 * precision) + recall)
F_0.5


