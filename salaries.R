obj=read.table("C:/Users/aditya royal/Downloads/salaries.txt",header=T)
#b)draw boxplots and summaries for each group
i=1
hist(subset(obj,City=='Athens')$Salary)
source('symmetricplot.R')
source('lval.R')
lval(subset(obj,City=='Athens')$Salary)
sym_plot(subset(obj,City=='Athens')$Salary,function(x) (x))
asdf=list()
for(x in unique(obj$City))
{
  
  asdf[i]=list(summary(subset(obj,obj[,'City']==x,select='Salary')))
  boxplot(c(subset(obj,obj[,'City']==as.character(x),select='Salary')))
  i=i+1
}
par(mfrow=c(1,1))
obj$Salary=(obj$Salary)^2
summary(subset(obj,obj[,'City']=='Hong_Kong',select='Salary'))
boxplot(c(subset(obj,obj[,'City']=='Hong_Kong',select='Salary')))
hist((subset(obj,obj[,'City']=='Singapore',select='Salary')$Salary)^2)
#c)using plots determine the power, I drawed mid summary plot
par(mfrow=c(2,3))
i=1
a=array(dim=c(4,6))
for (x in unique(obj$City)){
  if(length(lval(subset(obj,City==x)$Salary)$mids)==4){
    a[1:4,i]=lval(subset(obj,City==x)$Salary)$mids
  }
  if (length(lval(subset(obj,City==x)$Salary)$mids)==3){
    a[1:3,i]=lval(subset(obj,City==x)$Salary)$mids
  }
  #plot(a[1:4,i])
  i=i+1
}
obj$Salary=(obj$Salary)^2
source('hinkley.R')
hinkley((subset(obj,City=='Singapore')$Salary))
#c) I also drawed symmetry plot, now will determine the power of tansformation
# I believe log can neutralize the spread
source('symmetricplot.R')
i=1
par(mfrow=c(1,1))
sym_plot(subset(obj,City=='Singapore')$Salary,function(x) 1/x^2)

for (x in unique(obj$City)){
  sym_plot(subset(obj,City==x)$Salary,function(x) x^2)
  i=i+1
}

