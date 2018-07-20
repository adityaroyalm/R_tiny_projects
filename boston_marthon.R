marathon=read.csv('boston.marathon.wtimes.txt',sep ='\t',header=TRUE)
library('ggplot2')
plot(marathon$year,marathon$minutes, xlab="year", ylab="minutes")
qplot(data=marathon,year,minutes,geom=c('smooth'))+geom_point()
#1st
marathon$minutes=as.numeric(as.character(marathon$minutes))
smooth.3RSS<-smooth(marathon$minutes,kind='3RSS')
plot(marathon$year,marathon$minutes, xlab="year", ylab="minutes",main="3RSS SMOOTH")
with(marathon,                                      
     lines(year, smooth.3RSS, type="l",col="red",lwd=2, 
          ylim=c(120,180)))
h=5
##2nd
han=function(sequence)
{
  smooth=filter(sequence,c(1,2,1)/4)
  smooth[c(1,length(sequence))]=sequence[c(1,length(sequence))]
  return(smooth)
}

smooth.3RSSH <- han(smooth.3RSS) 
plot(marathon$year,marathon$minutes,xlab="year", ylab="minutes",main="3RSSH SMOOTH")
with(marathon,                                      
     lines(year, smooth.3RSSH, type="l",col="black",lwd=2, 
           ylim=c(120,180)))
     
#3rd

#install.packages("bootstrap")
library(bootstrap)
#running mean smoother

lines(ksmooth(marathon$year, marathon$minutes, bandwidth = h),col='red')
#runnig kernel smoother
lines(ksmooth(marathon$year, marathon$minutes, bandwidth = h, kernel = "normal"),col='green')


legend("topright", legend=c("3RSSh","running mean smoother","kernel smoother",'localpolynomial','spline'),lty=1, col=c('black','red','green','blue','pink'))


#running a local polynomial smooother 
#install.packages("KernSmooth")
library(KernSmooth)
lines(locpoly(marathon$year, marathon$minutes,  bandwidth = h),col='blue')
#spline
lines(smooth.spline(marathon$year, marathon$minutes, df = h),col='pink')
#redo 3
#
library('sets')
mean_smoother=ksmooth(marathon$year, marathon$minutes, bandwidth = h)
spline=smooth.spline(marathon$year, marathon$minutes, df = h)
locpoly=locpoly(marathon$year, marathon$minutes,  bandwidth = h)
selectedrows=asint(locpoly$x)
locpoly=locpoly[selectedrows,]
ksmooth=ksmooth(marathon$year, marathon$minutes, bandwidth = h)
H=c(5,10,20)
for x in H{
  
}
