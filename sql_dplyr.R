library(RSQLite)
library(dplyr)
library(DescTools)
##connect to db##
con<-dbConnect(SQLite(),dbname='C:/Users/aditya royal/Desktop/MLsoftaware_projects &datasets/database.sqlite')
##list all tables##
dbListTables(con)
country
League
match
##create a dataframe
player<-tbl_df(dbGetQuery(con,'SELECT * FROM Player'))
player_attributes<-tbl_df(dbGetQuery(con,'SELECT * FROM Player_Attributes'))
player_attributes<-player_attributes%>%
  rename(Player_Attributes_id=id)%>%
  left_join(player,by='player_api_id')
str(player_attributes)
player_attributes<-player_attributes%>%
  group_by(player_api_id)%>%
  top_n(n=1,wt=date)%>%
  as.data.frame() 
main<-player_attributes[order(player_attributes$overall_rating,decreasing = TRUE),]
main<-main%>%as.data.frame()
main[1:20,]
###########charts############
Desc(main[1:20,]$overall_rating,plotit=TRUE
main$Label
###########or####################
#chart<-group_by(main[1:20,],overall_rating)
#chart<-summarize(chart,count=n())
#plot(x=chart$overall_rating,y=chart$count)
#library(qtlcharts)
#iplotCorr(main[1:20,10:42],reorder=TRUE)
library(radarchart)
library(tidyr)
radardf<-main[1:20,]%>%select(player_name,10:42)%>%as.data.frame()
radardf<-gather(radardf,key=Label,value=Score,-player_name)%>%
  spread(key=player_name,value=Score)
radardf
chartJSRadar(scores=radardf)
