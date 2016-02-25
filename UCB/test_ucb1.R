library(ggplot2)
library(plyr)
library(reshape2)

#========================================================================
sim<-1000
times<-250
arms<-c(0.1,0.1,0.1,0.1,0.12)

source('UCB1.R')
result.ucb<-test_ucb1(arms, num_sims=sim, horizon=times)

#========================================================================
ucb.d<-data.frame(REWARD=result.ucb$RET,Times=1:times)
ucb.d$AverageReward<-ucb.d$REWARD/sim
ucb.d$CumSumReward<-cumsum(ucb.d$AverageReward)

# Plot average reward as a function of time.
qplot(Times,AverageReward,data=ucb.d,geom='line',main="plot average reward as a function of time")

# Plot cumulative reward as a function of time.
qplot(Times,CumSumReward,data=ucb.d,geom='line',main="plot average reward as a function of time")
