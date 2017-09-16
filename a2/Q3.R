mydata<- read.csv("pokemon.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)
#You perform preliminary analysis on the dataset and decide to drop 2variables from consideration for the PCA procedure.
#Which 2 variables aremost appropriate to bedropped? Give supporting reasons.
library("FactoMineR")
#required columns
mydata<-mydata[,c(5,6,7,8,9,10,11,12,16,20,21,22)]
names(mydata)
mydata<-mydata[order(mydata$Total, decreasing= T),]
#correlation matrix
cor.mat <- round(cor(mydata,use="pairwise", method="spearman"),2)
subset(melt(cor.mat), value > .5) #Highest correlation observed between height and weight. Thus removing them
mydata$Height_m<-NULL
mydata$Weight_kg<-NULL
names(mydata)
#view correlation matrix
library("corrplot")
corrplot(cor.mat, type="upper", order="hclust",tl.col="black", tl.srt=45)

#PCA
library("FactoMineR")
mydata_pca <- PCA(mydata, scale.unit = TRUE, graph = FALSE)  
mydata_pca

# Eigenvalues <=> amount of the variation explained by each principal component (PC).
eigenvalues <- mydata_pca$eig
print(eigenvalues)
eigen_sum<-sum(eigenvalues$eigenvalue)
for(i in 1:nrow(eigenvalues)){
  row<-eigenvalues[i,]
  x<-row$eigenvalue/eigen_sum
  print( c(i,row$eigenvalue,x) )
}
# If PC has eigenvalue > 1 then this PC accounts for more variance than accounted by one of the original variables in standardized data. 
# Using this as a cutoff point to determine the number of PCs to retain.

#Plot of variance assoiciated with each variable :
barplot(eigenvalues[, 2], names.arg=1:nrow(eigenvalues),  main = "Variances", xlab = "Principal Components",ylab = "Percentage of variances", col ="purple")
lines(x = 1:nrow(eigenvalues), eigenvalues[, 2],  type="b", pch=19, col = "black")

#Data to be stored in the new device will be the data related to the first 2 components

contribution<-as.data.frame(mydata_pca$var$contrib)
names(contribution)
#Finding the variable with maximum in Dimension 1
var1<-row.names(subset(contribution,contribution$Dim.1==max(contribution$Dim.1)))
print(var1)
#Finding the variable with maximum in Dimension 2
var2<-row.names(subset(contribution,contribution$Dim.2==max(contribution$Dim.2)))
print(var2)
#Data to be stored has to be only from these two columns
result_data<-subset(mydata,select=c(var1,var2))
print(result_data)


#Plot scatterplot based on first 2 principal components
mydata_pca <- PCA(mydata, scale.unit = TRUE, graph = FALSE)  
mydata_pca
#"$ind$coord"       "coord. for the individuals" 
PC1<-mydata_pca$ind$coord[,1]
PC2<-mydata_pca$ind$coord[,2]
PCs<-data.frame(cbind(PC1,PC2))

ggplot(PCs,aes(x=PC1,y=PC2)) + geom_point(colour="grey",size=2) +geom_point(data=PCs[1:10, ], aes(x=PC1, y=PC2), colour="black", size=2) + xlab("PC 1" )+ ylab("PC 2") + ggtitle("Scatterplot of first 2 pc")





