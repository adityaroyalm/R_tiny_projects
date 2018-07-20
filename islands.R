islands<-read.table("C:/Users/aditya royal/Downloads/island.areas.txt",header=T)
i=1
l=list()
#b)draw boxplots and summaries for each group
for(x in unique(islands$Ocean))
{

  l[i]=list(summary(subset(islands,islands[,'Ocean']==as.character(x),select='Area')))
  boxplot(c(subset(islands,islands[,'Ocean']==as.character(x),select='Area')))
  i=i+1
}
islands2=list()
islands2=islands$Area
islands$Area=1/sqrt(islands$Area)
hist(log(subset(islands,islands[,'Ocean']=='Indian',select='Area')$Area))

#par(mfrow=c(2,2))

list(summary(subset(islands,islands[,'Ocean']=='Indian',select='Area')))
boxplot(c(subset(islands,islands[,'Ocean']=='Indian',select='Area')))
islands$Area=islands2

source('hinkley.R')


source('lval.R')
hinkley((subset(islands,Ocean=='Mediterranean')$Area))


#c)using plots determine the power, I drawed mid summary plot
par(mfrow=c(2,2))
i=1
a=array(dim=c(4,5))
for (x in unique(islands$Ocean)){
  if(length(lval(subset(islands,Ocean==x)$Area)$mids)==4){
    a[1:4,i]=lval(subset(islands,Ocean==x)$Area)$mids
  }
  if (length(lval(subset(islands,Ocean==x)$Area)$mids)==3){
    a[1:3,i]=lval(subset(islands,Ocean==x)$Area)$mids
  }
  plot(a[1:4,i])
  i=i+1
}
#c) I also drawed symmetry plot, now will determine the power of tansformation
# I believe log can neutralize the spread
source('symmetricplot.R')
i=1
par(mfrow=c(2,2))
for (x in unique(islands$Ocean)){
  sym_plot(subset(islands,Ocean==x)$Area,function(x) x)
  i=i+1
}
#d)I will transform and do the steps from a to c again by aplying 1/sqrt to data
i=1
l[i]=list(summary(subset(islands,islands[,'Ocean']=='Caribbean',select='Area')))
boxplot(c(sqr(subset(islands,islands[,'Ocean']=='East_Indies',select='Area'))))
source('symmetricplot.R')
#2 need 1/sqrt(x) trnsformation tried till x^4 but still problem persisiting
l[i]=list(summary(subset(islands,islands[,'Ocean']=='East_Indies',select='Area')))
boxplot(c(1/sqrt(subset(islands,islands[,'Ocean']=='East_Indies',select='Area'))))

