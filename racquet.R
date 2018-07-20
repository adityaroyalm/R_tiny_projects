racq.data=read.table('racquet.txt',header=TRUE)
racq.data2=racq.data[,-1]
std=apply(racq.data2,2,sd)
racq.std=sweep(racq.data2,2,std,FUN='/')
dist_racq=dist(racq.std)
linkage=hclust(dist_racq,method='single')
plot(linkage,labels=racq.data$racquet,ylab='distance')




df=data.frame(racq.data2)
dist_mat=as.matrix(round(dist(df),5))
diag(dist_mat)=diag(dist_mat+1000)
st_1=which(dist_mat==min(dist_mat),arr.ind=TRUE)



km=kmeans(racq.std,centers=4,iter.max=100,nstart=25)
choices=2:7
n=length(racq.std[,1])
wss1=(n-1)*sum(apply(racq.std,2,var))
wss=numeric(0)
for(i in choices){
  w=sum(kmeans(racq.std,i)$withinss)
  wss=c(wss,w)
}





plot(c(choices),wss,type='l',xlab='number of clusters',ylab='within-groups susm-of-squares',lwd=2)
data.frame(racq.data[,1],km$cluster)
plot(km,which.plots=1)
###medoids
library('cluster')
racq.kmed=pam(dist(racq.data[-1]),k=4,diss=T)
plot(racq.kmed,which.plots=1)
