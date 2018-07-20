obj=read.table("C:/Users/aditya royal/Downloads/baseball.attendance.txt",header = 1)
stripchart(obj$Avg.Home,method='jitter')
summary(obj$Avg.Home)
hist(obj$Avg.Home)
qqnorm(obj$Avg.Home)
qqline(obj$Avg.Home)
summaries=list(list())
for(x in colnames(obj))
{summaries[x]=list(summary(obj[,x]))
}
hinkley=function(x)
{
 h= (mean(x)-median(x))/(diff(quantile(x,probs=c(0.25,0.75))))
 return(h)
}
hinkley(sqrt(obj$Avg.Home))
hinkley(log(obj$Avg.Home))
hist(sqrt(obj$Avg.Home))
