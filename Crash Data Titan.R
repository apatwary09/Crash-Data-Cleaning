
vv=filter(vwCollision$CountyStateCde,filter = 1)

vwCollision$CollisionDte <- as.Date(vwCollision$CollisionDte, format="%y-%m-%d")



vwCollision <- separate(vwCollision$CollisionDte,c("cYear","cMonth","cDay"),"-")
library(tidyr)
vwPersonViolation$Alc <- ifelse(vwPersonViolation$ViolationCategoryCde==1,1,0)
vwPersonViolation$viol <- ifelse(vwPersonViolation$ViolationCategoryCde>=1,1,0)
vwPersonViolation$recless <- ifelse(vwPersonViolation$ViolationCategoryCde==2,1,0)

vwPersonViolation$ViolationCategoryCde <- as.numeric(vwPersonViolation$ViolationCategoryCde)
vwPersonViolation$ViolationCategoryCde[is.na(vwPersonViolation$ViolationCategoryCde)] = 0
myvars <- c("MstrRecNbrTxt", "Alc", "viol", "recless")
VV_newdata <- vwPersonViolation[myvars]



vwCommercialUnit$VehicleConfigCde <- as.numeric(vwCommercialUnit$VehicleConfigCde)
vwCommercialUnit$ComUnit <- ifelse(vwCommercialUnit$VehicleConfigCde==99 | vwCommercialUnit$VehicleConfigCde==0,0,1)
vwCommercialUnit$ComUnit[is.na(vwCommercialUnit$ComUnit)] = 0


vwCommercialUnit$CarrierTypeCde <- as.numeric(vwCommercialUnit$CarrierTypeCde)
vwCommercialUnit$Interstate <- ifelse(vwCommercialUnit$CarrierTypeCde==1,1,0)
vwCommercialUnit$Interstate[is.na(vwCommercialUnit$Interstate)] = 0


C_myvars <- c("MstrRecNbrTxt", "ComUnit", "Interstate")
VC_newdata <- vwCommercialUnit[C_myvars]


CC_myvars <- c("MstrRecNbrTxt", "CollisionDte", "NC", "NbrFatalitiesNmb", "NbrInjuredNmb", "NbrNonInjuredNmb", "NbrMotoristsNmb","NbrNonMotoristsNmb", "CountyStateCde", "AlcoholInd","DrugInd", "HitRunInd", "SpeedInd")
Collision_newdata <- vwCollision[CC_myvars]



Collision_newdata$CD <- as.Date(Collision_newdata$CollisionDte, format="%y-%m-%d")
Collision_newdata$CD <- as.character.Date(Collision_newdata$CD)

library(tidyr)
Collision_newdata <- separate(Collision_newdata$CD,c("cYear","cMonth","cDay"),"-")


Collision_newdata$Colission_df <- data.frame(date = Collision_newdata$CD,
                 year = as.numeric(format(Collision_newdata$CD, format = "%Y")),
                 month = as.numeric(format(Collision_newdata$CD, format = "%m")),
                 day = as.numeric(format(Collision_newdata$CD, format = "%d")))

Collision_newdata$drugC <- ifelse(Collision_newdata$DrugInd== "Y",1,0)
Collision_newdata$drugC[is.na(Collision_newdata$drugC)] = 0

Collision_newdata$AlcoholC <- ifelse(Collision_newdata$AlcoholInd== "Y",1,0)
Collision_newdata$AlcoholC[is.na(Collision_newdata$AlcoholC)] = 0

Collision_newdata$HitC <- ifelse(Collision_newdata$HitRunInd== "Y",1,0)
Collision_newdata$HitC[is.na(Collision_newdata$HitC)] = 0

Collision_newdata$speedC <- ifelse(Collision_newdata$SpeedInd== "Y",1,0)
Collision_newdata$speedC[is.na(Collision_newdata$speedC)] = 0




Hey_CC_myvars <- c("MstrRecNbrTxt", "CollisionDte", "year","month", "NC", "NbrFatalitiesNmb", "NbrInjuredNmb", "NbrNonInjuredNmb", 
               "NbrMotoristsNmb","NbrNonMotoristsNmb", "CountyStateCde", "AlcoholC","drugC", "HitC", "speedC")
df10 <- Collision_newdata[Hey_CC_myvars]
df10$MstrRecNbrTxt <- as.numeric(df10$MstrRecNbrTxt)



library(lubridate)
Collision_newdata$year <- year(ymd(Collision_newdata$CD))
Collision_newdata$month <- month(ymd(Collision_newdata$CD)) 
Collision_newdata$day <- day(ymd(Collision_newdata$CD))


M1$MstrRecNbrTxt <- as.numeric(M1$MstrRecNbrTxt)


df10$MstrRecNbrTxt <- as.numeric(df10$MstrRecNbrTxt)
VC_newdata$MstrRecNbrTxt <- as.numeric(VC_newdata$MstrRecNbrTxt)
VV_newdata$MstrRecNbrTxt <- as.numeric(VV_newdata$MstrRecNbrTxt)

library(dplyr)

#Merging_Data
M1 <- full_join(VC_newdata, VV_newdata, by="MstrRecNbrTxt")

M2 <- full_join(M1, df10, by="MstrRecNbrTxt")














df1 = vwCollision[1:1000000,]
df2 = vwCollision[1000001:2000000,]
df3 = vwCollision[2000001:2385482,]

library(openxlsx)
library(dplyr)
library(rio)

export(df3, "DF3.csv")

GasSShop=aggregate(NC~CountyStateCde+year+month, data=vwCollision, FUN=sum)

vwCollision$NC <- rep(1,nrow(vwCollision))
df3$NCC <- rep(1,nrow(df3))
Exper1=aggregate(NCC~CountyStateCde+year+month, data=df3, FUN=sum)




#Total event
Exper1=aggregate(NC~CountyStateCde+year+month, data=M2, FUN=sum)


#NbrFatalitiesNmb
Exper2=aggregate(NbrFatalitiesNmb~CountyStateCde+year+month, data=M2, FUN=sum)


#NbrInjuredNmb
Exper3=aggregate(NbrInjuredNmb~CountyStateCde+year+month, data=M2, FUN=sum)


#NbrNonInjuredNmb
Exper4=aggregate(NbrNonInjuredNmb~CountyStateCde+year+month, data=M2, FUN=sum)


#NbrMotoristsNmb
Exper5=aggregate(NbrMotoristsNmb~CountyStateCde+year+month, data=M2, FUN=sum)

#NbrNonMotoristsNmb
Exper6=aggregate(NbrNonMotoristsNmb~CountyStateCde+year+month, data=M2, FUN=sum)

#Alcohol
Exper7=aggregate(AlcoholC~CountyStateCde+year+month, data=M2, FUN=sum)

#Drug
Exper8=aggregate(drugC~CountyStateCde+year+month, data=M2, FUN=sum)

#Hit
Exper9=aggregate(HitC~CountyStateCde+year+month, data=M2, FUN=sum)
#Speed
Exper10=aggregate(speedC~CountyStateCde+year+month, data=M2, FUN=sum)


#Alc
Exper11=aggregate(Alc~CountyStateCde+year+month, data=M2, FUN=sum)
#viol
Exper12=aggregate(viol~CountyStateCde+year+month, data=M2, FUN=sum)
#RecKless
Exper13=aggregate(recless~CountyStateCde+year+month, data=M2, FUN=sum)
#Interstate
Exper14=aggregate(Interstate~CountyStateCde+year+month, data=M2, FUN=sum)
#Interstate
Exper15=aggregate(ComUnit~CountyStateCde+year+month, data=M2, FUN=sum)






#Creating UID
Exper1$UID<- with(Exper1, paste0(CountyStateCde,year,month))
Exper2$UID<- with(Exper2, paste0(CountyStateCde,year,month))
Exper3$UID<- with(Exper3, paste0(CountyStateCde,year,month))
Exper4$UID<- with(Exper4, paste0(CountyStateCde,year,month))
Exper5$UID<- with(Exper5, paste0(CountyStateCde,year,month))
Exper6$UID<- with(Exper6, paste0(CountyStateCde,year,month))
Exper7$UID<- with(Exper7, paste0(CountyStateCde,year,month))
Exper8$UID<- with(Exper8, paste0(CountyStateCde,year,month))
Exper9$UID<- with(Exper9, paste0(CountyStateCde,year,month))
Exper10$UID<- with(Exper10, paste0(CountyStateCde,year,month))
Exper11$UID<- with(Exper11, paste0(CountyStateCde,year,month))
Exper12$UID<- with(Exper12, paste0(CountyStateCde,year,month))
Exper13$UID<- with(Exper13, paste0(CountyStateCde,year,month))
Exper14$UID<- with(Exper14, paste0(CountyStateCde,year,month))
Exper15$UID<- with(Exper15, paste0(CountyStateCde,year,month))





M1$MstrRecNbrTxt <- as.numeric(M1$MstrRecNbrTxt)


library(dplyr)

#Merging_Data
VC_newdata[is.na(VC_newdata)] = 0
VV_newdata[is.na(VV_newdata)] = 0
df10[is.na(df10)] = 0

M1 <- full_join(VC_newdata, VV_newdata, by="MstrRecNbrTxt")

M2 <- full_join(M1, df10, by="MstrRecNbrTxt")











#after_creating UID

md1 <- full_join(Exper1, Exper2, by="UID")
md2 <- full_join(md1, Exper3, by="UID")
md3 <- full_join(md2, Exper4, by="UID")
md4 <- full_join(md3, Exper5, by="UID")
md5 <- full_join(md4, Exper6, by="UID")
md6 <- full_join(md5, Exper7, by="UID")
md7 <- full_join(md6, Exper8, by="UID")
md8 <- full_join(md7, Exper9, by="UID")
md9 <- full_join(md8, Exper10, by="UID")
md10 <- full_join(md9, Exper11, by="UID")
md11 <- full_join(md10, Exper12, by="UID")
md12 <- full_join(md11, Exper13, by="UID")
md13 <- full_join(md12, Exper14, by="UID")
md14 <- full_join(md13, Exper15, by="UID")





AT[is.na(AT)] = 0

TD[is.na(TD)] = 0

AllData2 <- full_join(TD, AT, by="UID")







merged.data1 <- merge(merged.data, Exper4, by="UID")
merged.data2 <- merge(merged.data1, Exper5, by="UID")
merged.data3 <- merge(merged.data2, Exper6, by="UID")
merged.data4 <- merge(merged.data3, Exper7, by="UID")

library(openxlsx)
library(dplyr)
library(rio)

export(AllData2, "AllData2.csv")














#Panel compilation for Time Series Graph

#Alcohol
Preper7=aggregate(AlcoholC~year+month, data=M2, FUN=sum)

#Drug
preper8=aggregate(drugC~year+month, data=M2, FUN=sum)

#Hit
preper9=aggregate(HitC~year+month, data=M2, FUN=sum)
#Speed
preper10=aggregate(speedC~year+month, data=M2, FUN=sum)


#Alc
preper11=aggregate(Alc~year+month, data=M2, FUN=sum)
#viol
preper12=aggregate(viol~year+month, data=M2, FUN=sum)
#RecKless
preper13=aggregate(recless~year+month, data=M2, FUN=sum)
#Interstate
preper14=aggregate(Interstate~year+month, data=M2, FUN=sum)
#Interstate
preper15=aggregate(ComUnit~year+month, data=M2, FUN=sum)


#Creating UID

Preper7$UID<- with(Preper7, paste0(year,month))
preper8$UID<- with(preper8, paste0(year,month))
preper9$UID<- with(preper9, paste0(year,month))
preper10$UID<- with(preper10, paste0(year,month))
preper11$UID<- with(preper11, paste0(year,month))
preper12$UID<- with(preper12, paste0(year,month))
preper13$UID<- with(preper13, paste0(year,month))
preper14$UID<- with(preper14, paste0(year,month))
preper15$UID<- with(preper15, paste0(year,month))


#after_creating UID

tg7 <- full_join(Preper7, preper8, by="UID")
tg8 <- full_join(tg7, preper9, by="UID")
tg9 <- full_join(tg8, preper10, by="UID")
tg10 <- full_join(tg9, preper11, by="UID")
tg11 <- full_join(tg10, preper12, by="UID")
tg12 <- full_join(tg11, preper13, by="UID")
tg13 <- full_join(tg12, preper14, by="UID")
tg14 <- full_join(tg13, preper15, by="UID")




library(openxlsx)
library(dplyr)
library(rio)

export(tg14, "TimePlotsData.csv")



df10$MstrRecNbrTxt <- as.numeric(df10$MstrRecNbrTxt)
VC_newdata$MstrRecNbrTxt <- as.numeric(VC_newdata$MstrRecNbrTxt)
VV_newdata$MstrRecNbrTxt <- as.numeric(VV_newdata$MstrRecNbrTxt)


#Join
library(dplyr)

VC_newdata[is.na(VC_newdata)] = 0
VV_newdata[is.na(VV_newdata)] = 0
df10[is.na(df10)] = 0
