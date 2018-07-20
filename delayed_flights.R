obj<-read.csv('DelayedFlights.csv')
obj2=obj[apply(obj,1,function(x){!any(is.na(x))}),]

obj3=obj2[sample.int(1247488,100000),]
write.csv(obj3,file='cabi.csv')
library(dplyr)
table(obj3$UniqueCarrier)
obj4=obj3[obj3$Year==2008,]
obj5=select(obj4,c(-2,-14,-15,-17,-25,-7,-9,-24,-26,-27,-28,-29,-30))
write.csv(obj5,file='adi.csv')
h=names(unlist(list(table(obj5$Origin)>100))[which(unlist(list(table(obj5$Origin)>100)) %in% TRUE)])
obj6=obj5[obj5$Origin %in% h,]
s=names(unlist(list(table(obj6$Dest )>100))[which(unlist(list(table(obj6$Dest )>100)) %in% TRUE)])
obj7=obj6[obj6$Dest %in% s,]
table(obj7$UniqueCarrier)
o=names(unlist(list(table(obj7$UniqueCarrier)>200))[which(unlist(list(table(obj7$UniqueCarrier )>200)) %in% TRUE)])
obj7=obj7[obj7$UniqueCarrier %in% o,]
obj8=select(obj7,c(-TailNum,-FlightNum))
write.csv(obj8,'adiu.csv')
summary(lm(CRSArrTime ~.,data=obj8))
library(dummies)
obj8=cbind(obj8,dummy(obj8$UniqueCarrier,sep='_carri'))
obj8$UniqueCarrier=NULL
obj8=cbind(obj8,dummy(obj8$Origin ,sep='_origin'))
obj8$Origin=NULL
obj8=cbind(obj8,dummy(obj8$Dest,sep='_dest'))

obj8$Dest=NULL
table(obj8$Cancelled)
write.csv(obj8,'adit.csv')
