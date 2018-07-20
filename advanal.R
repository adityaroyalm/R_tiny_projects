input<-read.csv("Heart_imp.csv",header = TRUE)## Imputed data
library('dplyr')

head(input)
str(input)

summary(input)


input$sex<-as.factor(input$sex)
input$slope<-as.factor(input$slope)
input$fbs<-as.factor(input$fbs)
input$restecg<-as.factor(input$restecg)
input$exang<-as.factor(input$exang)
input$cp<-as.factor(input$cp)

str(input)

head(input)
library(mlr)

tI<-input
tI<-createDummyFeatures(tI,col="sex")
tI<-createDummyFeatures(tI,col="slope")
tI<-createDummyFeatures(tI,col="fbs")
tI<-createDummyFeatures(tI,col="restecg")
tI<-createDummyFeatures(tI,col="exang")
tI<-createDummyFeatures(tI,col="cp")

str(input)
tI$sex.0<-NULL
tI$slope.1<-NULL
tI$fbs.0<-NULL
tI$restecg.0<-NULL
tI$exang.0<-NULL
tI$cp.1<-NULL
str(tI)

library("MASS")
set.seed(123)
selection<-sample(seq_len(nrow(tI)),floor(0.75*nrow(tI)),replace=FALSE)
train<-tI[selection,]
test<-tI[-selection,]

full_train <- glm(num~.,family = binomial,data=train)
summary(full_train)
step<-stepAIC(full_train,trace=FALSE)
step$anova
new.full_train<-glm(num ~ slope.2 + chol + fbs.1 + restecg.1 + exang.1 + sex.1 + 
                cp.2 + cp.3 + oldpeak,family = binomial,data=train)
summary(new.full_train)

new.full_train$coefficients

test_x<-test
test_x$num<-NULL
testpreds=predict(new.full_train,test_x)
probs=exp(testpreds)/(1+exp(testpreds))
for (i in 1:length(probs)){
  if (probs[i]>0.6){
    test_y[i]=1
  }else{
    test_y[i]=0
  }
}
library(caret)
confusionMatrix(test$num,test_y)


########experiment

train_exp=train[,c('num','slope.2', 'chol','fbs.1','restecg.1','exang.1','sex.1', 'cp.2','cp.3','oldpeak')]
source("outside.R")
predicions <- log_reg(train_exp, size=10)
plot_pred_type_distribution(predicions, 0.6)
library(randomForest)
library(grid)
library(gridExtra)
roc <- calculate_roc(predictions, 1, 1, n = 100)
par(mfrow=c(1,1))
plot_roc(roc, 0.6, 1, 1)
