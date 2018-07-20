library(readxl)
my_df<-read_excel('C:/Users/aditya royal/Desktop/tfi.xlsx')
rand<-rbinom(119,1,.5)
my_df$rand=rand
my_df$final_x=my_df$rand*my_df$X_obs
my_df[my_df$final_x %in% 0,'final_x']=NA
my_df=subset(my_df,select=c('final_x','Y_obs','X_obs'))

model=lm(my_df$X_obs~my_df$Y_obs)
model2<-lm(my_df$Y_obs~my_df$X_obs)
plot(my_df$final_x,my_df$Y_obs)

df2<-subset(my_df,!is.na(final_x),select=c(final_x,Y_obs))

model2<-lm(df2$Y_obs~df2$final_x)
summary(model)
my_df[is.na(my_df$final_x),'final_x']=model$fitted.values[is.na(my_df$final_x)]
model3<-lm(my_df$Y_obs~my_df$final_x)
summary(model3)
