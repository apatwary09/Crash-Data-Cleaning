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
vwCommercialUnit$Intrastate <- ifelse(vwCommercialUnit$CarrierTypeCde==0,1,0)
vwCommercialUnit$Intrastate[is.na(vwCommercialUnit$Interstate)] = 0
C_myvars <- c("MstrRecNbrTxt", "ComUnit", "Interstate", "Intrastate")
VC_newdata <- vwCommercialUnit[C_myvars]



CC_myvars <- c("MstrRecNbrTxt", "CollisionDte", "NC", "NbrFatalitiesNmb", "NbrInjuredNmb", "NbrNonInjuredNmb", "NbrMotoristsNmb","NbrNonMotoristsNmb", "CountyStateCde","LightConditionCde", "WorkZoneTypeCde", "AlcoholInd","DrugInd", "HitRunInd", "SpeedInd")
Collision_newdata <- vwCollision[CC_myvars]

Collision_newdata$CD <- as.Date(Collision_newdata$CollisionDte, format="%y-%m-%d")

library(lubridate)
Collision_newdata$year <- year(ymd(Collision_newdata$CD))
Collision_newdata$month <- month(ymd(Collision_newdata$CD)) 
Collision_newdata$day <- day(ymd(Collision_newdata$CD))


Collision_newdata$FatalCrash <- ifelse(Collision_newdata$NbrFatalitiesNmb>=1,1,0)

Collision_newdata1 <- subset(Collision_newdata, FatalCrash ==1)

Collision_newdata1$drugC <- ifelse(Collision_newdata1$DrugInd== "Y",1,0)
Collision_newdata1$drugC[is.na(Collision_newdata1$drugC)] = 0

Collision_newdata1$AlcoholC <- ifelse(Collision_newdata1$AlcoholInd== "Y",1,0)
Collision_newdata1$AlcoholC[is.na(Collision_newdata1$AlcoholC)] = 0

Collision_newdata1$HitC <- ifelse(Collision_newdata1$HitRunInd== "Y",1,0)
Collision_newdata1$HitC[is.na(Collision_newdata1$HitC)] = 0

Collision_newdata1$speedC <- ifelse(Collision_newdata1$SpeedInd== "Y",1,0)
Collision_newdata1$speedC[is.na(Collision_newdata1$speedC)] = 0

Collision_newdata1$NonspeedC <- ifelse(Collision_newdata1$SpeedInd== "N",1,0)
Collision_newdata1$NonspeedC[is.na(Collision_newdata1$NonspeedC)] = 0


Collision_newdata1$LightConditionCde <- as.numeric(Collision_newdata1$LightConditionCde)
Collision_newdata1$DayLightC <- ifelse(Collision_newdata1$LightConditionCde== 1,1,0)
Collision_newdata1$DayLightC[is.na(Collision_newdata1$DayLightC)] = 0

Collision_newdata1$DarkNtLightedC <- ifelse(Collision_newdata1$LightConditionCde== 2,1,0)
Collision_newdata1$DarkNtLightedC[is.na(Collision_newdata1$DarkNtLightedC)] = 0

Collision_newdata1$DarkLightedC <- ifelse(Collision_newdata1$LightConditionCde== 3,1,0)
Collision_newdata1$DarkLightedC[is.na(Collision_newdata1$DarkLightedC)] = 0

Collision_newdata1$WorkZoneTypeCde <- as.numeric(Collision_newdata1$WorkZoneTypeCde)
Collision_newdata1$WorkZone <- ifelse(Collision_newdata1$WorkZoneTypeCde== 1|Collision_newdata1$WorkZoneTypeCde== 99,0,1)
Collision_newdata1$WorkZone[is.na(Collision_newdata1$WorkZone)] = 0




Hey_CCC_myvars <- c("MstrRecNbrTxt", "CollisionDte", "year","month", "NC", "NbrFatalitiesNmb", "NbrInjuredNmb", "NbrNonInjuredNmb", 
                   "NbrMotoristsNmb","NbrNonMotoristsNmb", "CountyStateCde", "AlcoholC","drugC", "HitC", "speedC", "NonspeedC"
                   ,"DayLightC", "DarkNtLightedC","DarkLightedC", "WorkZone")
df11 <- Collision_newdata1[Hey_CCC_myvars]





#Join
library(dplyr)


M1 <- full_join(VC_newdata, VV_newdata, by="MstrRecNbrTxt")

M2 <- left_join(df11,M1, by="MstrRecNbrTxt")





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


Exper16=aggregate(NonspeedC~CountyStateCde+year+month, data=M2, FUN=sum)
Exper17=aggregate(DayLightC~CountyStateCde+year+month, data=M2, FUN=sum)
Exper18=aggregate(DarkNtLightedC~CountyStateCde+year+month, data=M2, FUN=sum)
Exper19=aggregate(DarkLightedC~CountyStateCde+year+month, data=M2, FUN=sum)
Exper20=aggregate(WorkZone~CountyStateCde+year+month, data=M2, FUN=sum)
Exper21=aggregate(Intrastate~CountyStateCde+year+month, data=M2, FUN=sum)


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
Exper16$UID<- with(Exper16, paste0(CountyStateCde,year,month))
Exper17$UID<- with(Exper17, paste0(CountyStateCde,year,month))
Exper18$UID<- with(Exper18, paste0(CountyStateCde,year,month))
Exper19$UID<- with(Exper19, paste0(CountyStateCde,year,month))
Exper20$UID<- with(Exper20, paste0(CountyStateCde,year,month))
Exper21$UID<- with(Exper21, paste0(CountyStateCde,year,month))


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

md15 <- full_join(md14, Exper16, by="UID")
md16 <- full_join(md15, Exper17, by="UID")
md17 <- full_join(md16, Exper18, by="UID")
md18 <- full_join(md17, Exper19, by="UID")
md19 <- full_join(md18, Exper20, by="UID")
md20 <- full_join(md19, Exper21, by="UID")


library(openxlsx)
library(dplyr)
library(rio)

export(md20, "Char_CC_md20.csv")

