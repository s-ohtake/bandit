softmax<-function(horizon,temperature,value,count,exp_arms,annealing=F){    
    
    select_arm<-function(value,temperature){
        z <- sum(exp(value / temperature))
        probs <- exp(value / temperature) / z 
        p = runif(1,0,1)
        cum_prob <-cumsum(probs)
        select_arm<-which(cum_prob> p)[1]
        if(is.na(select_arm)){
            select_arm<-length(probs)
        }
        return(select_arm)
    }
    
    select_annealing_arm<-function(value,t){
        temperature = 1 / log(t + 0.0000001)        
        z <- sum(exp(value / temperature))
        probs <- exp(value / temperature) / z 
        p = runif(1,0,1)
        cum_prob <-cumsum(probs)
        select_arm<-which(cum_prob> p)[1]
        if(is.na(select_arm)){
            select_arm<-length(probs)
        }
        return(select_arm)
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
        if(annealing){
            chosen_arm<-select_annealing_arm(value,T)
        }else{
            chosen_arm<-select_arm(value,temperature)            
        }
        reward<-BernoulliArm(exp_arms[chosen_arm])                
        count[chosen_arm]<-count[chosen_arm] + 1
        new_value <- ((count[chosen_arm] - 1) / floor(count[chosen_arm])) * value[chosen_arm] + (1 / floor(count[chosen_arm])) * reward
        res_reward<-cbind(res_reward,reward)
        value[chosen_arm] <- new_value   
    }
    return(list(VALUES=value,COUNTS=count,REWARD=res_reward))
}

test_softmax<-function(exp_arms, num_sims=50, horizon=250,temperature=0.1,annealing=F){
    res_val<- matrix(ncol=length(exp_arms))[-c(1),]    
    res_count<- matrix(ncol=length(exp_arms))[-c(1),]    
    res_reward<-rep(0,horizon)
    for(T in 1:num_sims){
        count <- rep(0.0,length(exp_arms))
        value <- rep(0.0,length(exp_arms))    
        res<-softmax(horizon,temperature,value,count,exp_arms,annealing)
        res_reward<-res_reward+res$REWARD
    }
    return(list(RET=as.vector(res_reward),COUNT=res$COUNT,VALUE=res$VALUE))
}
