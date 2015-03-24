#HW 2 analysis
load("renal.Rdata")
naive<-t.test(toi~treat,data=renal[which(renal$time==3),])

#reshape to wide format to do ANCOVA
renalw<-reshape(renal,v.names="toi",idvar="id",times="time",direction="wide")
m1<-lm(toi.3~toi.1+treat,data=renalw)
summary(m1)

library(BDSS)#my home built package let me know if you would like a copy to reproduce my results
renalw$Treatment<-factor(ifelse(renalw$treat==0,"Control","Treatment"))
renalw$OffReas<-factor(renalw$offreas)
renalw$toimiss.1<-factor(ifelse(is.na(renalw$toi.1),"Yes","No"))
renalw$toimiss.2<-factor(ifelse(is.na(renalw$toi.2),"Yes","No"))
renalw$toimiss.3<-factor(ifelse(is.na(renalw$toi.3),"Yes","No"))
renalw$toimiss.4<-factor(ifelse(is.na(renalw$toi.4),"Yes","No"))

outtab<-SummaryTable(data=renalw,rowvars=c("OffReas",paste("toimiss",1:4,sep=".")),colvar="Treatment",cont.vars=c("toi.1","toi.2","toi.3","toi.4"),output="matrix")
write.csv(outtab,"HW2MissingTable.csv")

for(i in 1:dim(renalw)[1]){
  renalw$nmiss[i]<-with(renalw[i,],sum(c(toimiss.1=="Yes",toimiss.2=="Yes",toimiss.3=="Yes",toimiss.4=="Yes")))
}
renall<-reshape(renalw,direction="long")
library(ggplot2)
base<-ggplot(data=renall,aes(x=factor(time),y=toi,group=nmiss))+geom_point(aes(color=factor(nmiss)))+ylab("Mean TOI")+xlab("Time Point")+theme_bw()+scale_colour_manual("Number Missing\nObservations",values=c("purple","blue","red","black"))+geom_jitter(position=position_jitter(width=.1))
base+stat_summary(fun.data = "mean_cl_normal",geom="line")+aes(color=factor(nmiss))

rm(list=ls())
asm<-read.table("Asthma crossover.txt",header=F,skip=1,sep="")
names(asm)<-c("group","pat","prd","trt","pef","base")
library(lme4)#for mixed effect model
#random intercept model for patients
m2<-lmer(pef~trt+(1|pat),data=asm)
summary(m2)
#get conservative p-value since proper df are disputed in mixed effects models
1-pt(q=4.031,df=13)#of course I know you just want the 95% CI
#95% CI on the difference
t95<-qt(p=0.975,df=13)
lower<-m2@beta[2]-(t95*(coef(summary(m2))[, "Std. Error"][2]))
upper<-m2@beta[2]+(t95*(coef(summary(m2))[, "Std. Error"][2]))
m3<-lmer(pef~trt*group+(1|pat),data=asm)
summary(m3)
