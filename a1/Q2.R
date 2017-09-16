mydata<- read.csv("deliveries.csv",header=TRUE,sep=",")
mydata<-as.data.frame(mydata)

# compute  the  total  number  of  runs  scored  by  each  team  in  their  respective innings
result<-data.frame(NA,NA,NA,NA)
names(result)<-c("match_id","inning","batting_team","total_runs")

#Get a list of all match ids
allmatchid= unique(mydata$match_id)

mydataMatches<- read.csv("matches.csv",header=TRUE,sep=",")
mydataMatches<-as.data.frame(mydataMatches)
#All matches where dl_applied is 1
dl_appliedmatches=subset(mydataMatches,mydataMatches$dl_applied==1)
dl_appliedmatchids=unique(dl_appliedmatches$id)

#All matches where there is no result
noresultmatches<-subset(mydataMatches,mydataMatches$result=="no result")
noresultmatchids<-unique(noresultmatches$id)

for(matchid in allmatchid){
  #if the match_id does not belong to DL
  if(((matchid %in% dl_appliedmatchids)==FALSE) && ((matchid %in% noresultmatchids)==FALSE)){
    #Subset for this particular match
    current_match<-subset(mydata,mydata$match_id==matchid)
    #For each match get a list of innings
    allinnings= unique(current_match$inning)
    for(inn in allinnings){
      
      #Subset for this particular inning 
       each_inning=subset(current_match,current_match$inning==inn)
       #Total of column total_runs
       total<-sum(each_inning$total_runs)
       #slice first row to find batting team name
       x<-each_inning[1,]
       team=x$batting_team
       team<-toString(team)
       #Create new row with these values
       newrow<-data.frame(matchid,inn,team,total)
       names(newrow)<-c("match_id","inning","batting_team","total_runs")
       #append to result 
      result<-rbind(result,newrow)
       
    }
  }
  
}
orig_names<-names(result)
result<-data.frame(result[-1,])
names(result)<-orig_names



#Report your result for matches with match ID 7, 27, 67, 171 and 414.
actual_result <- subset(result, result$match_id==7 |result$match_id==27 |result$match_id==67|result$match_id==171|result$match_id==414)
actual_result<-unique(actual_result)
print(actual_result)



#Report  the team  that  scored  the  least  number  of  runs and  the team  thatscored  the maximum  number  of  runs  in  any  match  throughout  the  tournament along  with  their  scores.
minruns<-min(result$total_runs,na.rm = TRUE)
minrow<-subset(result,result$total_runs==minruns)
minrow<-unique(minrow)
print("min")
print(minrow)
print(minrow[c("batting_team","total_runs")])

maxruns<-max(result$total_runs,na.rm = TRUE)
maxrow<-subset(result,result$total_runs==maxruns)
maxrow<-unique(maxrow)
print("max")
print(maxrow[c("batting_team","total_runs")])
