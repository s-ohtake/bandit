epsilon_greedy<-function(horizon,epsilon,value,count,exp_arms,annealing=F){
    ChooseArm<-function(horizon,epsilon){
        choose<-rep(0,horizon)
        rond<-runif(horizon,0,1)
        choose[rond<=epsilon]<-1
        return(choose)
    }
    
    ChooseAnnealingArm<-function(horizon){
        choose<-rep(0,horizon)
        rond<-runif(horizon,0,1)
        epsilon <- 1 / log(1:horizon + 0.0000001)
        choose[rond<=epsilon]<-1
        return(choose)
    }
    
    select_arm<-function(cs,value){
        if(cs==0){
            return(which.max(value))
        }else{
            return(sample(length(value),1))
        }
    }
    BernoulliArm<-function(p){
        if(runif(1,0,1) > p){
            return(0)
        }else{
            return(1)        
        }
    }
    
    if(annealing){
        choose<-ChooseAnnealingArm(horizon)
    }else{
        choose<-ChooseArm(horizon,epsilon)            
    }
    res_reward<-c()
    T<-1
    for(cs in choose){
        chosen_arm<-select_arm(cs,value)
        reward<-BernoulliArm(exp_arms[chosen_arm])                
        count[chosen_arm]<-count[chosen_arm] + 1
        new_value <- ((count[chosen_arm] - 1) / floor(count[chosen_arm])) * value[chosen_arm] + (1 / floor(count[chosen_arm])) * reward
        res_reward<-cbind(res_reward,reward)
        value[chosen_arm] <- new_value   
        T<-T+1
    }
    return(list(VALUES=value,COUNTS=count,REWARD=res_reward))
}
test_epsilon_greedy<-function(exp_arms, num_sims=50, horizon=250,epsilon=0.1,annealing=F){
    res_val<- matrix(ncol=length(exp_arms))[-c(1),]    
    res_count<- matrix(ncol=length(exp_arms))[-c(1),]    
    res_reward<-rep(0,horizon)
    for(T in 1:num_sims){
        count <- rep(0.0,length(exp_arms))
        value <- rep(0.0,length(exp_arms))    
        res<-epsilon_greedy(horizon,epsilon,value,count,exp_arms,annealing)
        res_reward<-res_reward+res$REWARD
        gc()        
    }
    return(list(RET=as.vector(res_reward),COUNT=count,VAL=value))
}
