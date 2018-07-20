obj=read.table("C:/Users/aditya royal/Desktop/RAOS-Examples//Pyth/Pyth.dat",header=FALSE,skip=3)
model=lm(V1~V2+V3,data=obj)
summary(model)
par(mfrow=c(2,2))
plot(model)
outliers=subset(obj,obj$V1>5&obj$V3<5,select=c(V1,V2,V3))

predict.lm(model,obj[39:58,c('V2','V3')],interval = 'prediction')

library('foreign')
library('ggplot2')
obj<-read.dta('C:/Users/aditya royal/Downloads/child.iq.dta')
model=lm(obj$ppvt~obj$momage)
summary(model)
df1=subset(obj,obj$educ_cat==1,c('ppvt','momage'))
df2=subset(obj,obj$educ_cat==2,c('ppvt','momage'))
df3=subset(obj,obj$educ_cat==3,c('ppvt','momage'))
df4=subset(obj,obj$educ_cat==4,c('ppvt','momage'))
obj$educ_cat=as.factor(obj$educ_cat)
ggplot(obj,aes(y=ppvt,x=momage,color=educ_cat))+geom_point()+geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,
                                                                         method.args = list(start = list(b = 69.1554, a = 0.3433)))
par(mfrow=c(2,2))
plot(model)



obj<-read.dta('C:/Users/aditya royal/Downloads/child.iq.dta')
obj$educ_cat=as.factor(obj$educ_cat)
model=lm(obj$ppvt~obj$educ_cat+obj$momage)
summary(model)
df1=subset(obj,obj$educ_cat==1,c('ppvt','momage'))
df2=subset(obj,obj$educ_cat==2,c('ppvt','momage'))
df3=subset(obj,obj$educ_cat==3,c('ppvt','momage'))
df4=subset(obj,obj$educ_cat==4,c('ppvt','momage'))
obj$educ_cat=as.factor(obj$educ_cat)
ggplot(obj,aes(y=ppvt,x=momage,color=educ_cat))+geom_point()+geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,
                                                                         method.args = list(start = list(b=0,a=0)))
par(mfrow=c(1,1))
#plot(model)
obj$school=apply(obj,1,FUN = function(x) if(x[2] %in% c(2,3,4)) x[2]='SCHOOL' else x[2]='NO_SHOOL')
plot(obj$school,obj$momage)
obj<-subset(obj,select=-educ_cat)
model=lm(obj$ppvt~obj$school+obj$momage)
summary(model)

ggplot(obj,aes(y=ppvt,x=momage,color=school))+geom_point()+geom_smooth(method='lm')


obj<-read.dta('C:/Users/aditya royal/Downloads/child.iq.dta')
obj$educ_cat<-as.factor(obj$educ_cat)
obj_train<-obj[1:200,]/
  obj_test<-obj[200:400,c('educ_cat','momage')]
model=lm(obj_train$ppvt~obj_train$educ_cat+obj_train$momage)
summary(model)
predicted=predict.lm(model,obj_test)

plot(x=predicted,y=obj[201:400,'ppvt'],ylab = 'actual')
abline(a=0,b=1)
