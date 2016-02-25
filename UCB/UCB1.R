UCB1<-function(horizon,temperature,value,count,exp_arms){    
    select_arm<-function(values,counts){
        arm<-which(counts==0)[1]
        if(is.na(arm)){
            total_counts <- sum(counts)
            bonus <- sqrt((2 * log(total_counts)) / counts)
            ucb_values <- values + bonus
            arm<-which.max(ucb_values)
        }
        return(arm)
    }
    BernoulliArm<-function(p){
        if(runif(1,0,1) > p){
            return(0)
        }else{
            return(1)        
        }
    }   
    res_reward<-c()
    for(T in 1:horizon){
        chosen_arm<-select_arm(value,count) 
        reward<-BernoulliArm(exp_arms[chosen_arm])                
        count[chosen_arm]<-count[chosen_arm] + 1
        new_value <- ((count[chosen_arm] - 1) / floor(count[chosen_arm])) * value[chosen_arm] + (1 / floor(count[chosen_arm])) * reward
        res_reward<-cbind(res_reward,reward)
        value[chosen_arm] <- new_value   
    }
    return(list(VALUES=value,COUNTS=count,REWARD=res_reward))
}
test_ucb1<-function(exp_arms, num_sims=50, horizon=250){
    res_val<- matrix(ncol=length(exp_arms))[-c(1),]    
    res_count<- matrix(ncol=length(exp_arms))[-c(1),]    
    res_reward<-rep(0,horizon)
    for(T in 1:num_sims){
        count <- rep(0.0,length(exp_arms))
        value <- rep(0.0,length(exp_arms))    
        res<-UCB1(horizon,temperature,value,count,exp_arms)
        res_reward<-res_reward+res$REWARD
        gc()
        
    }
    return(list(RET=as.vector(res_reward),COUNT=res$COUNT,VALUE=res$VALUE))
}
