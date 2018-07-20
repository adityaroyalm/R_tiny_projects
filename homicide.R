library(dplyr)
library(plyr)
library(magrittr)
homicide=read.csv('C:/Users/aditya royal/Desktop/MLsoftaware_projects/database.csv')
head(homicide)
states=unique(homicide$State)
count_state=list()
#count_state=[lambda x:]
#lapply(states,function(x) for i in homicide$State if x==homicide$State[i] x.value=x.value+1)
names(homicide)<-tolower(names(homicide))
data.frame(table(homicide$State))
count(homicide$State[homicide$State=='Utah'])
weapon=unique(homicide$weapon)
dict<-list()
for (state in states){
   for (i in 1:length(weapon)){
      dict[[state]][[i]]=length(homicide$weapon[homicide$state==state & homicide$weapon==weapon[i]])
   }
}
for (element in dict){
      names(dict[element])<-weapon
}

homicide[,c(1,2)]
dict$Wyoming
###filtering###
homicide.solved<-homicide%>%filter(crime.solved=='Yes'& victim.sex!='Unknown' & perpetrator.sex!='Unknown' & relationship!='Unknown')
#homicide$whokilled<-ifelse(homicide.solved$relationship=='Brother'&homicide.solved$perpetrator.sex='Male')droplevels()
#summarizin#
by.yeaer<-summarize(group_by(homicide,year),n=n())
########PLots######
ex.h.ex.w<-ggplot(exhusband.ex.wife.crime ,aes(x=relationship,fill=relationship))
+geom_bar