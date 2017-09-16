mydata<- read.csv("pokemon.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

#Calculate the distance between means of Pokémons of primary type, Grass and fire
#columns to be considered are : HP, Attack,defence,Sp_Atk,Sp_Def, speed
grass=subset(mydata,mydata$Type_1=="Grass")
grass<-grass[c("HP",	"Attack","Defense","Sp_Atk","Sp_Def","Speed")]
fire=subset(mydata,mydata$Type_1=="Fire")
fire<-fire[c("HP",	"Attack","Defense","Sp_Atk","Sp_Def","Speed")]

#Normalizing data to range between 0 and 1
NormalizeData0to1<-function(colname){
  max_val<-max(colname)
  min_val<-min(colname)
  normalized<-data.frame()
  for(i in 1:nrow(colname)){
    y<-colname[i,]
    z=(y - min_val)/(max_val-min_val)
    normalized<-rbind(normalized,z)
  }
  return(normalized)
}

#Creating a normalized data set
grass_normalized<-data.frame(matrix(nrow = nrow(grass)))
for(i in 1:ncol(grass)){
  current_col<-grass[i]
  normalized_col<-NormalizeData0to1(current_col)
  grass_normalized<-cbind(grass_normalized,normalized_col)
}
grass_normalized<-grass_normalized[,-1]
names(grass_normalized)<-names(grass)
grass_normalized

fire_normalized<-data.frame(matrix(nrow = nrow(fire)))
for(i in 1:ncol(fire)){
  current_col<-fire[i]
  normalized_col<-NormalizeData0to1(current_col)
  fire_normalized<-cbind(fire_normalized,normalized_col)
}
fire_normalized<-fire_normalized[,-1]
names(fire_normalized)<-names(fire)
fire_normalized

#CALCULATING DIFFERENCES
g1mean<-mean(grass_normalized$HP)
f1mean<-mean(fire_normalized$HP)
HP_mean_diff<-g1mean-f1mean

g2mean<-mean(grass_normalized$Attack)
f2mean<-mean(fire_normalized$Attack)
Attack_mean_diff<-g2mean-f2mean

g3mean<-mean(grass_normalized$Defense)
f3mean<-mean(fire_normalized$Defense)
Def_mean_diff<-g3mean-f3mean

g4mean<-mean(grass_normalized$Sp_Atk)
f4mean<-mean(fire_normalized$Sp_Atk)
Spa_mean_diff<-g4mean-f4mean

g5mean<-mean(grass_normalized$Sp_Def)
f5mean<-mean(fire_normalized$Sp_Def)
Spd_mean_diff<-g5mean-f5mean

g6mean<-mean(grass_normalized$Speed)
f6mean<-mean(fire_normalized$Speed)
Speed_mean_diff<-g6mean-f6mean

print("mean differences")
print(names(grass_normalized))
print(c(HP_mean_diff,Attack_mean_diff,Def_mean_diff,Spa_mean_diff,Spd_mean_diff,Speed_mean_diff))

result<-data.frame(NA,NA)
names(result)<-c("Grass means","Fire means")
gm<-c(g1mean,g2mean,g3mean,g4mean,g5mean,g6mean)
fm<-c(f1mean,f2mean,f3mean,f4mean,f5mean,f6mean)
allnames<-names(grass_normalized)
result<-data.frame(allnames,gm,fm)
result

library(ggplot2)
#Line graph of their means
ggplot(data=result, aes(x=allnames, y=gm, group=1)) +  geom_line() +xlab("Factor") + ylab("Mean Values") + ggtitle("Grass Type")
ggplot(data=result, aes(x=allnames, y=fm, group=1)) +  geom_line() +xlab("Factor") + ylab("Mean Values") + ggtitle("Fire Type")


#Kurtosis and skew of heights of pokemons
all_pokemon_weights<-mydata$Weight_kg
all_pokemon_weights
all_pokemon_heights<-mydata$Height_m
all_pokemon_heights

library(moments)
weight_skew<-skewness(all_pokemon_weights)
height_skew<-skewness(all_pokemon_heights)
weight_kurtosis<-kurtosis(all_pokemon_weights)
height_kurtosis<-kurtosis(all_pokemon_heights)
print("Skew : Weight and Height")
print(c(weight_skew,height_skew))
print("Kurtosis : Weight and Height")
print(c(weight_kurtosis,height_kurtosis))


#Normalize dataset with mean 0 and standard deviation 1
NormalizeData_Mean0SD1<-function(colname){
  colmean<-sapply(colname,mean)
  colsd<-sapply(colname,sd)
  normalized<-data.frame()
  for(i in 1:nrow(colname)){
    y<-colname[i,]
    z=(y - colmean)/colsd
    #Z score is (value-mean)/sd
    normalized<-rbind(normalized,z)
  }
  return(normalized)
}

required_columns<-mydata[c("HP","Attack", "Defense","Sp_Atk","Sp_Def","Speed","Total","Height_m","Weight_kg")]
#Normalizing every column
normalized_columns<-data.frame(matrix(nrow=nrow(mydata)))
for(i in 1:ncol(required_columns)){
  current_col<-required_columns[i]
  #For each column, normalize the data
  normalized_col<-NormalizeData_Mean0SD1(current_col)
  normalized_columns<-cbind(normalized_columns,normalized_col)
}
normalized_columns<-normalized_columns[,-1]
normalized_columns
names(normalized_columns)<-names(required_columns)
library(reshape2)
df1<-melt(normalized_columns["HP"])
p1<-ggplot (df1, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)
df2<-melt(normalized_columns["Attack"])
p2<-ggplot (df2, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)
df3<-melt(normalized_columns["Defense"])
p3<-ggplot (df3, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)

df4<-melt(normalized_columns["Sp_Atk"])
p4<-ggplot (df4, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)
df5<-melt(normalized_columns["Sp_Def"])
p5<-ggplot (df5, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)
df6<-melt(normalized_columns["Speed"])
p6<-ggplot (df6, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)

df7<-melt(normalized_columns["Total"])
p7<-ggplot (df7, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)
df8<-melt(normalized_columns["Height_m"])
p8<-ggplot (df8, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)
df9<-melt(normalized_columns["Weight_kg"])
p9<-ggplot (df9, mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)

multiplot(p1,p2,p3,p4,p5,p6,p7,p8,p9,cols=3)

ggplot (melt(normalized_columns), mapping = aes (color = variable, x = value)) + geom_density (alpha = 0.5) + xlim(-5,7)+ geom_vline(xintercept=0)+geom_hline(yintercept=0)





