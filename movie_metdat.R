library(dplyr)
library(ggplot2)
library(data.table)
library(plotly)
library(formattable)
movie_metadata<-read.csv('C:/Users/aditya royal/Desktop/MLsoftaware_projects &datasets/movie_metadata.csv')
#movie_metadata %>%
#   select(movie_title,title_year) %>% group_by(title_year)%>%
 ## plot_ly(x=title_year,y=n)
str(movie_metadata)
temp<-movie_metadata%>%select(movie_title,title_year)
temp<-temp%>%group_by(title_year)%>%summarise(n=n())
temp<-na.omit(temp)
p<-ggplot(data=temp,aes(x=title_year,y=n))+geom_line()
ggplotly(p)

#which directors had highest rating#
#sorting director names by imdb scores#
temp<-movie_metadata%>%select(director_name,imdb_score)%>%group_by(director_name)%>%summarize(avg_score=mean(imdb_score))
temp<-temp[order(temp$avg_score,decreasing = TRUE),]
temp[1:20,]
#,we can not use sort for pipes
#qplot(title_year,n,data=temp,geom='point')
#plot_ly(temp,x=~title_year,y=~n)
#p %>%
  #add_trace(y = fitted(loess(n ~ as.numeric(title_year))), x = title_year) %>%
  #layout(title = "Year and Movies",showlegend = FALSE) %>%
  #dplyr::filter(n == max(n)) %>%
  #layout(annotations = list(x = title_year, y = n, text = "Peak", showarrow = T))
