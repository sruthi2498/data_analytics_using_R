mydata<- read.csv("deliveries.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

#‘Suresh  Raina is a key player of Gujarat Lions and  his performance in every match is directly reflected in the total score of the team’ 

mydataMatches<- read.csv("matches.csv",header=TRUE,sep=",")
mydataMatches<-as.data.frame(mydataMatches)

#Function to return matchid and runs scored in that match
total_runs_scored<-function(dataset){
  result<-data.frame(NA,NA)
  names(result)<-c("match_id","total_runs")
  
  allmatchids<-unique(dataset$match_id)
  for(matchid in allmatchids){
    current_match<-subset(dataset,dataset$match_id==matchid)
    totalruns<-sum(current_match$total_runs)
    newrow<-data.frame(matchid,totalruns)
    names(newrow)<-c("match_id","total_runs")
    result<-rbind(result,newrow)
  }
  result<-data.frame(result[-1,])
  return(result)
}

#Total runs scored data of Gujarat Lions
GL<-subset(mydata,mydata$batting_team=="Gujarat Lions")
GL_dataset<-total_runs_scored(GL)

#All matches of Gujarat Lions that they won
GL_win<-subset(mydataMatches,mydataMatches$winner=="Gujarat Lions")
GL_win_match_ids<-unique(GL_win$id)

#Removing matches that they did not win
result<-data.frame(NA,NA)
names(result)<-c("match_id","total_runs")
for(matchid in GL_win_match_ids) {
  newrow<-subset(GL_dataset,GL_dataset$match_id==matchid)
  result<-rbind(result,newrow)
}
result<-data.frame(result[-1,])
GL_dataset<-result

#Total runs scored data of Suresh Raina in Gujarat Lions
SR<-subset(GL,GL$batsman=="SK Raina")
SR_dataset<-total_runs_scored(SR)

#Removing matches that they did not win
result<-data.frame(NA,NA)
names(result)<-c("match_id","total_runs")
for(matchid in GL_win_match_ids) {
  newrow<-subset(SR_dataset,SR_dataset$match_id==matchid)
  result<-rbind(result,newrow)
}
result<-data.frame(result[-1,])
SR_dataset<-result

#Merging both datasets on match id
result<-merge(GL_dataset,SR_dataset,by="match_id")
result<-result[order(-result$total_runs.y),]
result
