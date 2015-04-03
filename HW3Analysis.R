library(BDSS)
load("renal.Rdata")

#reshape to wide format to do ANCOVA
renalw<-reshape(renal,v.names="toi",idvar="id",times="time",direction="wide")

#question 3
library(contrast)
library(multcomp)
library(lme4)
renal$ftime<-factor(as.character(renal$time))
renal$friskgrp<-factor(renal$riskgrp)
# Question 2
m1<-lmer(toi~ftime*treat*friskgrp + (1|id), data=renal)
m1b<-lm(toi~ftime*treat*friskgrp, data=renal)#need this to generate the contrast skeleton
#compare treatment by risk group interactions
cc2<-contrast::contrast(m1b,a=list(friskgrp="1",treat=1,ftime="1"),b=list(friskgrp="0",treat=0, ftime="1"))
cc2$X[1:24]<-c(rep(0,4),1,rep(0,11),1,0,rep(1/3,3),rep(0,3))# group 1 vs 0
trt.grp1v0<-summary(glht(m1,linfct=cc2$X))
trt.grp1v0
cc2$X[1:24]<-c(rep(0,4),1,rep(0,11),0,1,rep(0,3),rep(1/3,3))# group 2 vs 0
trt.grp2v0<-summary(glht(m1,linfct=cc2$X))
trt.grp2v0
cc2$X[1:24]<-c(rep(0,4),1,rep(0,11),1,-1,rep(1/3,3),rep(-1/3,3))# group 2 vs 1
trt.grp2v1<-summary(glht(m1,linfct=cc2$X))
trt.grp2v1


#Question 3
m2<-lmer(toi~ftime*treat+(1|id),data=renal)
m2b<-lm(toi~ftime*treat,data=renal)#needed to get the contrast matrix since the lmer method is a bit funky

ccA<-contrast::contrast(m2b,a=list(ftime="3",treat=1), b=list(ftime="3",treat=0))
estA<-summary(glht(m2, linfct=ccA$X))
resultsA<-matrix(c(estA$test$coef,estA$test$sigma,estA$test$coef-(qt(.975,563)*estA$test$sigma),estA$test$coef+(qt(.975,563)*estA$test$sigma)),1,4)
colnames(resultsA)<-c("Estimate","SE","Lower 95% CI","Upper 95% CI")
resultsA

ccB$X<-ccA$X
ccB$X[6:8]<-c(1/3,1/3,1/3)
estB<-summary(glht(m2, linfct=ccB$X))
resultsB<-matrix(c(estB$test$coef,estB$test$sigma,estB$test$coef-(qt(.975,563)*estB$test$sigma),estB$test$coef+(qt(.975,563)*estB$test$sigma)),1,4)
colnames(resultsB)<-colnames(resultsA)
resultsB



#Question 4
renalw$survmnths<-renalw$survtime/30 #convert to months
sv1<-Surv(renalw$survmnths, event=renalw$dead)
sf1<-survfit(sv1~treat,data=renalw)

ggSurvival(sf1, spot.cens=T, CI=T, xlab="Survival Months", stratName="Arm", groupNames=c("Control","Treatment"),groupNameLevels=c("Control","Treatment"))#this is a home-built plotting function

svtest<-survdiff(sv1~treat,data=renalw, rho=0) #rho=0 corresponds to log-rank test
svtest


#Question 5
sf1 #print method gives median survival times and 95% CIs

#Question 6
renalw$friskgrp<-factor(renalw$riskgrp)
d<-datadist(renalw)
options(datadist="d")
mcph<-cph(sv1~treat+friskgrp,data=renalw)
mcph
summary(mcph)

#question 7
m2cph<-cph(sv1~treat*friskgrp,data=renalw)
m2cph
summary(m2cph)
