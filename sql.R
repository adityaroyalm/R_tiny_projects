library(RSQLite)
library(dplyr)
con<-dbConnect(SQLite(),dbname='C:/Users/aditya royal/Desktop/MLsoftaware_projects &datasets/database.sqlite')
player<-tbl_df(dbGetQuery(con,'SELECT * FROM player'))
match<-tbl_df(dbGetQuery(con,'SELECT * FROM Match'))
team<-tbl_df(dbGetQuery(con,'SELECT * FROM Team'))
country<-tbl_df(dbGetQuery(con,'SELECT * FROM Country'))
League<-tbl_df(dbGetQuery(con,'SELECT * FROM League'))
player_attributes<-tbl_df(dbGetQuery(con,'SELECT * FROM Player_Attributes'))
player<-select(player,player_name,player_api_id)
team<-select(team,team_api_id,team_long_name,team_short_name)
Country<-select(Country,id,name)
League<-select(League,country_id,name)%>%rename(League_name=name)
match<-select(match, id, country_id, league_id, season, stage, date, match_api_id, home_team_api_id, away_team_api_id, home_team_goal, away_team_goal, home_player_1, home_player_2, home_player_3, home_player_4, home_player_5, home_player_6, home_player_7, home_player_8, home_player_9, home_player_10, home_player_11, away_player_1, away_player_2, away_player_3, away_player_4, away_player_5, away_player_6, away_player_7, away_player_8, away_player_9, away_player_10, away_player_11, goal, shoton, shotoff, foulcommit, card, cross, corner, possession)
Keycols<-c("season","league_id","home_teen_api_id")
library(data.table)
PointsDF<-match%>%
  select(1:11)%>%
  mutate(homepoint=if_else((home_team_goal>away_team_goal),3,if_else((home_team_goal==
                                                                       away_team_goal),1,0)))%>%
  mutate(awayPoint=if_else((home_team_goal>away_team_goal),0,if_else((home_team_goal==away_team_goal),1,3)))
tableHomeDt<-PointsDF%>%
  group_by(season,league_id,home_team_api_id)%>%
  summarise(pointsHome=sum(homepoint))%>%
  ungroup() %>% data.table
  Keycols = c('season','league')
