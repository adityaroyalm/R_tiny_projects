obj<-read.csv("C:/Users/aditya royal/Downloads/avgpm25.csv")
library(ggplot2)
library(ggmap)
library(maps)
#pm 2.5 above 15 oulers longitude and latitude
pm25above15=obj[(obj$pm25>15&obj$region=='west'),c('longitude','latitude','fips')]
#pm2.5 below outliers longitude and latitude
pm25less6=obj[(obj$pm25<6&obj$region=='east'),c('longitude','latitude','fips')]
#identifying counties 
df=data.frame(matrix(nrows=576,ncols=5))
df=merge(obj,county.fips,by='fips')
#counties having pm25 greater than 15
counties_above=df[df$fips %in% pm25above15$fips,'polyname']
#counties having pm25 less than 6
counties_below=df[df$fips %in% pm25less6$fips,]
#drawing the lon,lat locations on map
map=get_map(location=c(mean(rbind(pm25above15,pm25less6)$longitude),mean(rbind(pm25above15,pm25less6)$latitude)+15),zoom=3,maptype='terrain',scale=1,crop = TRUE)
ggmap(map)+geom_point(data=rbind(pm25above15,pm25less6),aes(x=longitude,y=latitude,fill='red'))


