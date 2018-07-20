install.packages("HH")
install.packages('nortest')# checking for normality
install.packages('car')#ncv test (non constant variance test)
install.packages('lmtest')#Durbin Watson test to check correlation b/w error terms
install.packages('alr3')#lack of fit test(linear model assumption)
install.packages("dummies")
install.packages("Hmisc")
j=1
for (i in colnames(concrete.ds)){
  store[[j]]=ggplot(concrete.ds,aes_string(as.character(i),'CCS'))+ geom_point()
  print(store[[j]])
  j=j+1
}
library(alr3)
library(car)
library(lmtest)
library(nortest)
library(HH)
library(dummies)
library(Hmisc)

concrete.ds=read.csv("Concrete_Data_Clean.csv")
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Age,sep='_age'))
concrete.ds$Age=NULL
concrete.ds$concrete.ds_age1=NULL
dim(concrete.ds)
head(concrete.ds)
attach(concrete.ds)


str(concrete.ds)

summary(concrete.ds)
summary(concrete.lm)



# performing all tests


plot(concrete.ds)
cor(concrete.ds)

summary(concrete.lm1)
ggplot(concrete.ds,aes())

concrete.ds$Cement_cut=cut2(concrete.ds$Cement,g=20)
concrete.ds$Blast.Furnace.Slag_cut=cut2(log(1+concrete.ds$Blast.Furnace.Slag),g=20)
concrete.ds$Fly.Ash_cut=cut2(log(1+concrete.ds$Fly.Ash),g=20)
concrete.ds$Water_cut=cut2(log(1+concrete.ds$Water),g=20)
concrete.ds$Coarse.Aggregate_cut=cut2(log(1+concrete.ds$Coarse.Aggregate),g=20)
concrete.ds$Fine.Aggregate_cut=cut2(log(1+concrete.ds$Fine.Aggregate),g=20)
concrete.ds$Cement=NULL
concrete.ds$Blast.Furnace.Slag=NULL
concrete.ds$Fly.Ash=NULL
concrete.ds$Water=NULL
concrete.ds$Fine.Aggregate=NULL
concrete.ds$Coarse.Aggregate=NULL
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Cement_cut,sep='cement_'))
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Blast.Furnace.Slag_cut,sep='BFS_'))
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Fly.Ash_cut,sep='ash_'))
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Water_cut,sep='water_'))
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Fine.Aggregate_cut,sep='Fine_'))
concrete.ds=cbind(concrete.ds,dummy(concrete.ds$Coarse.Aggregate_cut,sep='coarse_'))

concrete.ds$Cement_cut=NULL
concrete.ds$Blast.Furnace.Slag_cut=NULL
concrete.ds$Fly.Ash_cut=NULL
concrete.ds$Water_cut=NULL
concrete.ds$Fine.Aggregate_cut=NULL
concrete.ds$Coarse.Aggregate_cut=NULL

concrete.lm=lm(CCS~.,data=concrete.ds)
concrete.Mean <- lm(CCS~1,data=concrete.ds)
concrete.Stepwise <- step(concrete.Mean,
                          scope = formula(concrete.lm), direction = "both")
concrete.ds$Cement_cut=NULL
concrete.ds$Blast.Furnace.Slag_cut=NULL
concrete.ds$Fly.Ash_cut=NULL
concrete.ds$Water_cut=NULL
concrete.ds$Fine.Aggregate_cut=NULL
concrete.ds$Coarse.Aggregate_cut=NULL
concrete.lm1=lm(sqrt(CCS) ~ concrete.ds_age3 + `concrete.dscement_[144,154)` + concrete.ds_age7 + 
                  concrete.dsash_0.00 + `concrete.dscement_[485,540]` + concrete.dsBFS_0.0000 + 
                  `concrete.dscement_[154,166)` + `concrete.dscement_[102,144)` + 
                  concrete.ds_age14 + concrete.ds_age28 + `concrete.dscement_[355,380)` + 
                  `concrete.dscement_[393,428)` + `concrete.dscement_[428,485)` + 
                  `concrete.dsBFS_[3.2189,4.0037)` + `concrete.dscoarse_[6.74,6.75)` + 
                  `concrete.dsBFS_[5.2704,5.4714)` + `concrete.dswater_[5.23,5.23)` + 
                  `concrete.dsFine_[6.50,6.54)` + `concrete.dswater_[5.33,5.47)` + 
                  `concrete.dscement_[314,333)` + `concrete.dscement_[380,393)` + 
                  `concrete.dscement_[333,355)` + `concrete.dsBFS_[0.0198,3.2189)` + 
                  `concrete.dswater_[5.28,5.31)` + `concrete.dsash_[3.24,4.51)` + 
                  `concrete.dscement_[166,178)` + `concrete.dscement_[178,194)` + 
                  `concrete.dswater_[5.09,5.11)` + concrete.ds_age100 + `concrete.dscement_[194,213)` + 
                  `concrete.dscement_[222,238)` + `concrete.dsBFS_[5.4714,5.8872]` + 
                  `concrete.dswater_[5.14,5.17)` + `concrete.dswater_[5.31,5.33)` + 
                  `concrete.dswater_[4.81,4.99)` + `concrete.dswater_[5.07,5.09)` + 
                  `concrete.dsash_[4.86,4.96)` + `concrete.dsBFS_[5.2364,5.2704)` + 
                  `concrete.dswater_[4.99,5.05)` + `concrete.dsBFS_[5.1053,5.2364)` + 
                  `concrete.dswater_[5.17,5.19)` + `concrete.dswater_[5.05,5.07)` + 
                  `concrete.dswater_[5.11,5.14)` + `concrete.dsFine_[6.39,6.42)` + 
                  `concrete.dscement_[213,222)` + `concrete.dsFine_[6.57,6.60)` + 
                  `concrete.dsash_[5.15,5.30]` + `concrete.dsFine_[6.54,6.57)` + 
                  `concrete.dsFine_[6.69,6.70)` + `concrete.dscoarse_[6.94,6.96)` + 
                  `concrete.dscement_[288,304)` + `concrete.dscoarse_[6.78,6.82)` + 
                  `concrete.dsBFS_[4.0037,4.5933)` + `concrete.dscoarse_[6.69,6.74)` + 
                  `concrete.dscoarse_[6.92,6.94)` + `concrete.dsFine_[6.74,6.75)` + 
                  `concrete.dscoarse_[6.96,6.97)` + `concrete.dscoarse_[7.01,7.04]` + 
                  `concrete.dscement_[304,314)` + `concrete.dscement_[273,288)` + 
                  `concrete.dsash_[4.83,4.86)` + concrete.ds_age270 + `concrete.dsFine_[6.42,6.50)` + 
                  `concrete.dsFine_[6.70,6.72)` + `concrete.dsFine_[6.72,6.74)` + 
                  concrete.ds_age365 + concrete.ds_age91 + `concrete.dswater_[5.27,5.28)` + 
                  `concrete.dsBFS_[4.9767,5.1053)` + `concrete.dswater_[5.23,5.24)` + 
                  concrete.ds_age56
                ,data=concrete.ds)

plot(concrete.lm1)

vif(concrete.lm1)
# performing all tests
pureErrorAnova(concrete.lm1)
ncvTest(concrete.lm1)
dwtest(concrete.lm1)
ad.test(concrete.lm1$residuals)

ggplot(concrete.ds,aes(Water_cut,CCS))+geom_point()




total_columns=c('Cement' , 'concrete.ds_age3' ,'concrete.ds_age7' , 'Blast.Furnace.Slag'  
                  ,'Water' , 'concrete.ds_age28' , 'concrete.ds_age14', 'Fly.Ash', 
                  'concrete.ds_age100' , 'concrete.ds_age270' , 'concrete.ds_age365', 
                  'Fine.Aggregate' , 'Coarse.Aggregate')
plot_list=which(as.character((colnames(concrete.ds))) %in% total_columns)
store=list()
j=1
# use for loop to plot all variables without defining each column explicitly
for (i in plot_list){
  store[[j]]=ggplot(concrete.ds,aes_string(colnames(concrete.ds)[i],'CCS'))+ geom_point()
  print(store[[j]])
  j=j+1
}
ggplot(concrete.ds,aes(Blast.Furnace.Slag,Fly.Ash))+geom_jitter()
plot(Cement_cut + concrete.ds_age3 + Water_cut + Blast.Furnace.Slag_cut + 
       concrete.ds_age7 + concrete.ds_age100 + concrete.ds_age56 + 
       Fine.Aggregate_cut + concrete.ds_age14 + concrete.ds_age28 + 
       Coarse.Aggregate_cut + Fly.Ash_cut + concrete.ds_age91 + 
       concrete.ds_age270 + concrete.ds_age365)
