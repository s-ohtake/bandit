library(ggplot2)
library(plyr)
library(reshape2)

source('epsilon_greedy.R')

#========================================================================
sim<-1000
times<-250
arms<-c(0.1,0.2,0.5,0.8,0.9)
result.anil<-test_epsilon_greedy(arms, num_sims=sim, horizon=times,annealing=T)
result.ep.0.1<-test_epsilon_greedy(arms, num_sims=sim, horizon=times,epsilon=0.1)
result.ep.0.2<-test_epsilon_greedy(arms, num_sims=sim, horizon=times,epsilon=0.2)
result.ep.0.3<-test_epsilon_greedy(arms, num_sims=sim, horizon=times,epsilon=0.3)
result.ep.0.4<-test_epsilon_greedy(arms, num_sims=sim, horizon=times,epsilon=0.4)
result.ep.0.5<-test_epsilon_greedy(arms, num_sims=sim, horizon=times,epsilon=0.5)
#========================================================================
ep.gr.d<-rbind(
        data.frame(Epsilon="anneal",REWARD=result.anil$RET),
        data.frame(Epsilon="0.1",REWARD=result.ep.0.1$RET),
        data.frame(Epsilon="0.2",REWARD=result.ep.0.2$RET),
        data.frame(Epsilon="0.3",REWARD=result.ep.0.3$RET),
        data.frame(Epsilon="0.4",REWARD=result.ep.0.4$RET),
        data.frame(Epsilon="0.5",REWARD=result.ep.0.5$RET))
ep.gr.d$T<-c(rep(1:times,6))

# Plot average reward as a function of time.
ep.gr.d$AVG<-ep.gr.d$REWARD/sim
qplot(T,AVG,data=ep.gr.d,geom='line',colour=Epsilon,main="plot average reward as a function of time")

# Plot cumulative reward as a function of time.
tmp<-as.data.frame(t(aggregate(AVG ~ Epsilon, ep.gr.d[,-c(2,3)], cumsum)[,-c(1)]))
colnames(tmp)<-c("anneal","0.1","0.2","0.3","0.4","0.5")
tmp$T<-1:times
stats<-melt(tmp, id="T",value.name='CumsumReward',variable.name='Epsilon')
res<-merge(ep.gr.d,stats)
qplot(T,CumsumReward,data=res,geom='line',colour=Epsilon,main="Plot cumulative reward as a function of time")
