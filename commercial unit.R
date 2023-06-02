



vwCollision$NC <- rep(1,nrow(vwCollision))

Ped_custom_CC_myvars <- c("MstrRecNbrTxt", "CollisionDte","NC", "NbrFatalitiesNmb","CountyStateCde", "WeatherCde")
ForPed_Collision_newdata <- vwCollision[Ped_custom_CC_myvars]

ForPed_Collision_newdata$CD <- as.Date(ForPed_Collision_newdata$CollisionDte, format="%y-%m-%d")


library(lubridate)
ForPed_Collision_newdata$year <- year(ymd(ForPed_Collision_newdata$CD))
ForPed_Collision_newdata$month <- month(ymd(ForPed_Collision_newdata$CD)) 
ForPed_Collision_newdata$day <- day(ymd(ForPed_Collision_newdata$CD))

ForPed_Collision_newdata$WeatherCde <- as.numeric(ForPed_Collision_newdata$WeatherCde)
ForPed_Collision_newdata$WeatherCde <- ifelse(ForPed_Collision_newdata$WeatherCde==2 | ForPed_Collision_newdata$WeatherCde==3| ForPed_Collision_newdata$WeatherCde==4| ForPed_Collision_newdata$WeatherCde==5| ForPed_Collision_newdata$WeatherCde==6| ForPed_Collision_newdata$WeatherCde==7
                                              | ForPed_Collision_newdata$WeatherCde==8| ForPed_Collision_newdata$WeatherCde==9| ForPed_Collision_newdata$WeatherCde==10,1,0)








vwUnit$BodyCde[is.na(vwUnit$BodyCde)] = 0
vwUnit$BodyCde <- as.numeric(vwUnit$BodyCde)
vwUnit$BodyCde <- ifelse(vwUnit$BodyCde==50|vwUnit$BodyCde==51|vwUnit$BodyCde==52|vwUnit$BodyCde==58|vwUnit$BodyCde==59,1,0)
GG=sum(vwUnit$BodyCde)

VW <- subset(vwUnit, BodyCde ==1)

vw_Vars= c("MstrRecNbrTxt","BodyCde")
VW1 = VW[vw_Vars]

T1 <- left_join(VW1,ForPed_Collision_newdata, by="MstrRecNbrTxt")

Data_NC=aggregate(NC~CountyStateCde+year+month, data=T1, FUN=sum)
Data_Fatality=aggregate(NbrFatalitiesNmb~CountyStateCde+year+month, data=T1, FUN=sum)

library(openxlsx)
library(dplyr)
library(rio)

export(Data_NC, "NC_Bus.csv")

export(Data_Fatality, "Fatal_Bus.csv")




vwCommercialUnit$CarrierTypeCde <- as.numeric(vwCommercialUnit$CarrierTypeCde)
vwCommercialUnit$Interstate <- ifelse(vwCommercialUnit$CarrierTypeCde==1,1,0)
vwCommercialUnit$Interstate[is.na(vwCommercialUnit$Interstate)] = 0
vwCommercialUnit$Intrastate <- ifelse(vwCommercialUnit$CarrierTypeCde==0,1,0)
vwCommercialUnit$Intrastate[is.na(vwCommercialUnit$Interstate)] = 0

vwCommercialUnit$ComHazmat <- ifelse(vwCommercialUnit$VehicleConfigCde==70 | vwCommercialUnit$VehicleConfigCde==80,1,0)




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

Unit_myvars <- c("MstrRecNbrTxt", "VehicleOperationTypeCde", "RoadAlignmentCde", "RoadProfileCde")
UU_newdata <- vwUnit[Unit_myvars]

UU_newdata$VehicleOperationTypeCde <- as.numeric(UU_newdata$VehicleOperationTypeCde)
UU_newdata1$RoadAlignmentCde <- as.numeric(UU_newdata1$RoadAlignmentCde)
UU_newdata1$RoadProfileCde <- as.numeric(UU_newdata1$RoadProfileCde)

UU_newdata$truck <- ifelse(UU_newdata$VehicleOperationTypeCde>=1,1,0)

UU_newdata1 <- subset(UU_newdata, VehicleOperationTypeCde ==2)


UU_newdata1$CurveRoad <- ifelse(UU_newdata1$RoadAlignmentCde==2|UU_newdata1$RoadAlignmentCde==3,1,0)

UU_newdata1$SlopeRoad <- ifelse(UU_newdata1$RoadProfileCde==1,0,1)



vwCommercialUnitHAZMAT$Number_ComHazmat <- rep(1,nrow(vwCommercialUnitHAZMAT))

Haz_vars <- c("MstrRecNbrTxt","Number_ComHazmat")
VW_Hazmat <- vwCommercialUnitHAZMAT[Haz_vars]

M15 <- left_join(VW_Hazmat, ForPed_Collision_newdata, by="MstrRecNbrTxt")
Data_Hazmat=aggregate(Number_ComHazmat~CountyStateCde+year+month, data=M15, FUN=sum)



M21 <- left_join(UU_newdata1, vwCommercialUnit, by="MstrRecNbrTxt")


M22 <-left_join(M21, VW_Hazmat, by="MstrRecNbrTxt")

M23 <-left_join(M22, ForPed_Collision_newdata, by="MstrRecNbrTxt")

vwPersonViolation$ViolationCategoryCde <- as.numeric(vwPersonViolation$ViolationCategoryCde)
vwPersonViolation$ViolationCategoryCde[is.na(vwPersonViolation$ViolationCategoryCde)] = 0

vwPersonViolation$ViolationCategoryCde <- ifelse(vwPersonViolation$ViolationCategoryCde>=1,1,0)


Viol_vars <- c("MstrRecNbrTxt","ViolationCategoryCde")
VW_viol <- vwPersonViolation[Viol_vars]
M24 <-left_join(M23, VW_viol, by="MstrRecNbrTxt")

library(dplyr)


Data_ComUnit=aggregate(ComUnit~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Interstate=aggregate(Interstate~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Intrastate=aggregate(Intrastate~CountyStateCde+year+month, data=M24, FUN=sum)
Data_NC=aggregate(NC~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Fatality=aggregate(NbrFatalitiesNmb~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Weather=aggregate(WeatherCde~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Hazmat=aggregate(Number_ComHazmat~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Curve=aggregate(CurveRoad~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Slope=aggregate(SlopeRoad~CountyStateCde+year+month, data=M24, FUN=sum)
Data_Viol=aggregate(ViolationCategoryCde~CountyStateCde+year+month, data=M24, FUN=sum)


library(openxlsx)
library(dplyr)
library(rio)

export(Data_ComHazmat, "Data_ComHazmat.csv")





#Creating UID
Data_ComUnit$UID<- with(Data_ComUnit, paste0(CountyStateCde,year,month))
Data_Interstate$UID<- with(Data_Interstate, paste0(CountyStateCde,year,month))
Data_Intrastate$UID<- with(Data_Intrastate, paste0(CountyStateCde,year,month))
Data_NC$UID<- with(Data_NC, paste0(CountyStateCde,year,month))
Data_Fatality$UID<- with(Data_Fatality, paste0(CountyStateCde,year,month))
Data_Weather$UID<- with(Data_Weather, paste0(CountyStateCde,year,month))
Data_Viol$UID<- with(Data_Viol, paste0(CountyStateCde,year,month))
Data_Hazmat$UID<- with(Data_Hazmat, paste0(CountyStateCde,year,month))
Data_Curve$UID<- with(Data_Curve, paste0(CountyStateCde,year,month))
Data_Curve$UID<- with(Data_Slope, paste0(CountyStateCde,year,month))





md1 <- full_join(Data_ComUnit, Data_Interstate, by="UID")
md2 <- full_join(md1, Data_Intrastate, by="UID")
md3 <- full_join(md2, Data_NC, by="UID")
md4 <- full_join(md3, Data_Fatality, by="UID")
md5 <- full_join(md4, Data_Weather, by="UID")
md6 <- full_join(md5, Data_Viol, by="UID")
md7 <- full_join(md6, Data_Curve, by="UID")
md8 <- full_join(md7, Data_Slope, by="UID")
md9 <- full_join(md8, Data_Hazmat, by="UID")






md9$Intrastate[is.na(md9$Intrastate)] = 0







#after_creating UID

md1 <- full_join(Data_Bicyclists, Data_Alc, by="UID")
md2 <- full_join(md1, Data_BusinessC, by="UID")
md3 <- full_join(md2, Data_SchoolC, by="UID")
md4 <- full_join(md3, Data_ResidentC, by="UID")
md5 <- full_join(md4, Data_ComHazmat, by="UID")
md6 <- full_join(md5, Data_DarkLightedC, by="UID")
md7 <- full_join(md6, Data_DayLightC, by="UID")
md8 <- full_join(md7, Data_DarkNtLightedC, by="UID")
md9 <- full_join(md8, Data_drugC, by="UID")
md10 <- full_join(md9, Data_NC, by="UID")
md11 <- full_join(md10, Data_HitC, by="UID")
md12 <- full_join(md11, Data_Interstate, by="UID")
md13 <- full_join(md12, Data_Intrastate, by="UID")
md14 <- full_join(md13, Data_NmbrFatality, by="UID")

md15 <- full_join(md14, Data_Pedestrian, by="UID")
md16 <- full_join(md15, Data_R_FrontageRouteC, by="UID")
md17 <- full_join(md16, Data_R_InterstateC, by="UID")
md18 <- full_join(md17, Data_R_MunicipalRouteC, by="UID")
md19 <- full_join(md18, Data_R_StateRouteC, by="UID")
md20 <- full_join(md19, Data_R_USRouteC, by="UID")

md21 <- full_join(md20, Data_recless, by="UID")
md22 <- full_join(md21, Data_speedC, by="UID")
md23 <- full_join(md22, Data_speedC, by="UID")
md24 <- full_join(md23, Data_viol, by="UID")
md25 <- full_join(md24, Data_WorkZone, by="UID")
md26 <- full_join(md25, Exper27, by="UID")
md27 <- full_join(md26, Exper28, by="UID")
md28 <- full_join(md27, Exper29, by="UID")
md29 <- full_join(md28, Exper30, by="UID")

Data_ComUnit$UID<- as.numeric(Data_ComUnit$UID)

M16 <- full_join(M15, Data_ComUnit, by="UID")

M16$ComUnit[is.na(M16$ComUnit)] = 0

library(openxlsx)
library(dplyr)
library(rio)

export(md9, "TruckInvolved.csv")






















