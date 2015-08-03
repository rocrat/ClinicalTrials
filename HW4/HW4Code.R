#HW 4 code
library(car)
library(lme4)
library(lmerTest)

share<-read.csv("./HW4/SHARE_txt.csv")
share$sex<-factor(ifelse(share$sex==1,"Male", "Female"))
share$arm<-factor(ifelse(share$arm==0,"Control","Intervention"))
share$debut<-factor(Recode(share$debut, "0='No';1='Yes'"))
share$regret<-factor(Recode(share$regret, "0='No';1='Yes'"))

#Question 1
m1<-lmer(kscore~arm+(1|school),data=share)
KRsumm<-lmerTest::summary(m1,ddf="Kenward-Roger")
KR.t<-qt(.975,df=KRsumm$coef[2,3])
KR.95CI<-round(c(KRsumm$coef[2,1]-KR.t*KRsumm$coef[2,2],KRsumm$coef[2,1]+KR.t*KRsumm$coef[2,2]),2)
KR.95CI

#Question 2
schoolVar<-summary(m1)$varcor$school[1]
errorVar<-(summary(m1)$sigma)^2
icc<-schoolVar/(schoolVar+errorVar)
icc

#Question 3
#calculate adjusted mean cluster size
n<-dim(share)[1]
m.prime<-(1/length(levels(factor(share$school))))*(n-(sum(summary(factor(share$school))^2)/n))
#calculate design effect
des.eff<-1+(m.prime-1)*icc
des.eff

#Question 4
#aggregate responses at the cluster level
clust.level<-aggregate(share$kscore, list(school=share$school, arm=share$arm), mean, na.rm=T)
m2<-lm(x~arm,data=clust.level)#fit a linear model (equivalent to a t-test)
summ2<-summary(m2)
m2.95CI<-round(c(summ2$coef[2,1]-qt(.975,df=summ2$df[2])*summ2$coef[2,2], summ2$coef[2,1]+qt(.975,df=summ2$df[2])*summ2$coef[2,2]),2)

#Question 5
#create sample size function for comparing means
ssize<-function(alpha, beta, sigma, k, margin){
  n2<- (qnorm(1 - alpha/2) + qnorm(1 - beta))^2 * sigma^2 * (1 + 1/k)/margin^2
  n1<-k*n2
  return(list(N=ceiling(n1+n2),N1=ceiling(n1),N2=ceiling(n2)))
}
ssize(alpha=.05, beta=.2, k=2, sigma=1, margin=.2)

#Question 6
#create sample sie function for comparing proportions
pssize<-function(alpha, beta, p1, p2){
  delta<-abs(p1-p2)
  n <- (qnorm(1 - alpha/2) + qnorm(1 - beta))^2 * (p1 * (1 -  p1) + p2 * (1 - p2))/delta^2
  N<-2*n
  return(ceiling(N))
}
N<-pssize(alpha=.02, beta=.18, p1=.47, p2=.37)
#adjust for twins
N<-1.12*N
#adjust for attrition
N<-(.17*N)+N
#using 82% power gets me the closest

#Question 7
#calculate pooled variance
p.sigma<-sqrt(.5*(3.5^2+4^2))
#Calculate initial sample size with pooled variance
careN<-ssize(alpha=.05, beta=.1, sigma=p.sigma, k=1, margin=2)
careN
d.eff<-1+(30-1)*.03
#account for attrition
careN<-lapply(careN,FUN=function(x){(x*.2)+x})
#correct intervention arm for clustering
careN$N2<-ceiling(careN$N2*d.eff)
careN[2:3]

#Question 8
#using the formula for a 95% confidence interval on a proportion and the fact that the variance is maximized at p=.5 we get:
PilotN<-ceiling((.5*.5)/(.1/1.96)^2)
PilotN
