library(blockrand)
dat<-list()
m<-c("Male","Female")
center<-c(1,2,3,4)
strat<-expand.grid(m,center)

for(i in 1:8){
  dat[[i]]<-blockrand(n=125,num.levels=2,levels=c("Treatment","Control"))
  dat[[i]]$Sex<-strat[i,1]
  dat[[i]]$Center<-strat[i,2]
  dat[[i]]<-dat[[i]][1:125,]
}
data<-do.call(rbind,dat)
sum(data$treatment=="Treatment")
table(data$Center,data$treatment)
table(data$Sex,data$treatment)

trtA<-trtB<-trtC<-vector()
trtGroup<-character()
i<-0
while(length(trtGroup)<600 ){
  u<-runif(1)
  if(u < (1/3) & length(trtA)<200){
    i<-i+1
    trtGroup[i]<-"Treatment A"
    trtA<-c(trtA,1)
  }
  if(u > (1/3) & u < (2/3) & length(trtB)<200){
    i<-i+1
    trtGroup[i]<-"Treatment B"
    trtB<-c(trtB,1)
  }
  if(u > (2/3) & length(trtC)<200){
    i<-i+1
    trtGroup[i]<-"Treatment C"
    trtC<-c(trtC,1)
  }
}
sum(trtGroup=="Treatment A")
sum(trtGroup=="Treatment B")
