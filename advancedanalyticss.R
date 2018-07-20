library('foreign')
df=read.dta('C:/Users/aditya royal/Downloads/heights.dta')
df$weight=df$height
df$height=0.083*df$height2+df$height1
df[is.na(df$earn),'earn']=median(df$earn)
df[is.na(df$height),'height']=median(df$height)
df[is.na(df$weight),'weight']=median(df$weight)
df$earn[which(df$earn %in% 0)]=6000
df$male=2-df$sex
df$male=as.factor(df$male)
plot(df$height,df$earn)
model=lm(log(df$earn)~df$height+df$male)
summary(model)


df<-read.dta('C:/Users/aditya royal/Downloads/pollution.dta')
plot(log(df$nox),df$mort)
model<-lm(df$mort~log(df$nox))
summary(model)
plot(model)

model<-lm(df$mort~log(df$nox)+log(df$so2)+log(df$hc))
summary(model)
plot(df$mort[1:30],model$fitted.values[1:30],xlab='actual',ylab='predicted')
abline(a=0,b=1)
plot(df[30:60,'mort'],predict(model,df[,c('nox','so2','hc')])[30:60],xlab='actual',ylab='predicted')
abline(a=0,b=1)
