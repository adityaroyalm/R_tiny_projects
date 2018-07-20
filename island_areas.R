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

par(mfrow=c(1,1))
source('lval.R')
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

for (x in unique(islands$Ocean)){
  sym_plot(subset(islands,Ocean==x)$Area,log)
  i=i+1
}
#d)I will transform and do the steps from a to c again by aplying log to data
for (x in unique(islands$Ocean)){
  sym_plot(log(subset(islands,Ocean==x)$Area))
  i=i+1
}
source('symmetricplot')
#Acrctic is fine 2,3 need 1/sqrt(x) trnsformation
for (x in unique(islands$Ocean)){
  sym_plot(subset(islands,Ocean==x)$Area,log)
  i=i+1
}
# for 2,3 1/sqrt(x) trnsformation
sym_plot(1/(sqrt(subset(islands,Ocean=='Indian')$Area)))

#source('list_function.R')
#curve(powert(islands$Area,-1))


#su=seq(0,50,3)
#lines(su,su,type='l')
