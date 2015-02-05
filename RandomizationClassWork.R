library(blockrand)
dat<-list()
m<-c(1,2)
center<-c(1,2,3,4)
strat<-expand.grid(m,center)

for(i in 1:8){
  dat[[i]]<-blockrand(n=125,num.levels=2,levels=c("Treatment","Control"))
  dat[[i]]$Sex<-strat[i,1]
  dat[[i]]$Center<-strat[i,2]
  dat[[i]]<-dat[[i]][1:125,]
}
data<-do.call(rbind,dat)

plotblockrand(data,'C:\\Classes\\ClinicalTrials\\ExampleRand.pdf',
              top)

#Sanity Checks
sum(data$Sex==1)
sum(data$treatment=="Treatment")
