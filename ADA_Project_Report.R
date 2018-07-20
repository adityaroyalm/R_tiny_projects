Heart=read.delim(file.choose(), sep=",")
head(Heart)
summary(Heart)
attach(Heart)
#concentrating on missing values#
library(mice)
install.packages("mice")
install.packages("VIM")
install.packages("ggplot2")
install.packages("HH")
require("HH")
library(Hmisc)
library(VIM)
library(ggplot2)
library(dplyr)
library(psych)
library(reshape2)
library(tidyr)
md.pattern(Heart)
p=md.pairs(Heart);p

#dropping variables ca & tha - missing values are > 90%

Heart_nw=data.frame(slope,chol,fbs,trestbps,restecg,thalach,exang,age,sex,cp,oldpeak,num)

#missing value pattern for new heart data

md.pattern(Heart_nw)
p=md.pairs(Heart_nw);p
head(Heart_nw)
dim(Heart_nw)
str(Heart_nw)
mice_plot <- aggr(Heart_nw, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(Heart_nw), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))
imputed_Data <- mice(Heart_nw, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)
completedata2=complete(imputed_Data,2)
head(completedata2)
summary(completedata2)

#storing data into excel file#
library(xlsx)
library(foreign)
write.csv(completedata2, "C:/Users/charanjasti/Desktop/Summer Sem/Heart_imp.csv")


