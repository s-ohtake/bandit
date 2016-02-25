library(ggplot2)
library(plyr)
library(reshape2)

source('softmax.R')


#========================================================================
sim<-1000
times<-250
arms<-c(0.9,0.9,0.7,0.8,0.9)

result.sm.anil<-test_softmax(arms, num_sims=sim, horizon=times,annealing=T)
result.sm.0.1<-test_softmax(arms, num_sims=sim, horizon=times,temperature=0.1)
result.sm.0.2<-test_softmax(arms, num_sims=sim, horizon=times,temperature=0.2)
result.sm.0.3<-test_softmax(arms, num_sims=sim, horizon=times,temperature=0.3)
result.sm.0.4<-test_softmax(arms, num_sims=sim, horizon=times,temperature=0.4)
result.sm.0.5<-test_softmax(arms, num_sims=sim, horizon=times,temperature=0.5)
#========================================================================
softmax.d<-data.frame(Temperature="anneal",REWARD=result.sm.anil$RET)
softmax.d$T<-1:times

softmax.d<-rbind(
    data.frame(Temperature="anneal",REWARD=result.sm.anil$RET),
    data.frame(Temperature="0.1",REWARD=result.sm.0.1$RET),
    data.frame(Temperature="0.2",REWARD=result.sm.0.2$RET),
    data.frame(Temperature="0.3",REWARD=result.sm.0.3$RET),
    data.frame(Temperature="0.4",REWARD=result.sm.0.4$RET),
    data.frame(Temperature="0.5",REWARD=result.sm.0.5$RET))
softmax.d$T<-c(rep(1:times,6))

# Plot average reward as a function of time.
softmax.d$AVG<-softmax.d$REWARD/sim
qplot(T,AVG,data=softmax.d,geom='line',colour=Temperature,main="plot average reward as a function of time")

# Plot cumulative reward as a function of time.
stats<-as.data.frame(t(aggregate(AVG ~ Temperature, softmax.d[,-c(2,3)], cumsum)[,-c(1)]))
colnames(stats)<-c("anneal","0.1","0.2","0.3","0.4","0.5")
stats$T<-1:times
stats<-melt(stats, id="T",value.name='CumsumReward',variable.name='Temperature')
res<-merge(softmax.d,stats)

qplot(T,CumsumReward,data=res,geom='line',colour=Temperature,main="Plot cumulative reward as a function of time")
