#analysis of peanut study data
load("C:/Classes/ClinicalTrials/Peanut.rdata")
nut<-Peanut
outcome<-ifelse(Peanut$Outcome_of_Peanut_OFC_at_V60__ch=="Allergic",2,ifelse(Peanut$Outcome_of_Peanut_OFC_at_V60__ch=="Indeterminate",1,ifelse(Peanut$Outcome_of_Peanut_OFC_at_V60__ch=="Tolerant",0,NA)))

nut$Overall_V60_Outcome__OFC___Indet<-ifelse(nut$Overall_V60_Outcome__OFC___Indet=="NA",NA,nut$Overall_V60_Outcome__OFC___Indet)
nut$outcome<-as.numeric(as.factor(nut$Overall_V60_Outcome__OFC___Indet))-1

table(nut[nut$Stratum_char_=="SPT-negative Stratum",]$outcome)
tbs<-table(nut$Treatment_Group__Char_,nut$Overall_V60_Outcome__OFC___Indet,nut$Stratum__char_)
mantelhaen.test(tbs)
library(epiR)
equal<-epi.2by2(tbs)
summary(equal)

m1<-glm(outcome~Treatment_Group__Char_*Stratum__char_,family="binomial",data=nut)
summary(m1)
m2<-glm(outcome~Treatment_Group__Char_,family="binomial",data=nut)
summary(m2)

library(fmsb)
riskdifference(tbs[1,1,1],tbs[2,1,1],sum(tbs[1,,1]),sum(tbs[2,,1]))
