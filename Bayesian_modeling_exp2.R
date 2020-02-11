library(dplyr)

dat <- read.csv("Experiment_2/dat_exp2.csv")
rps_predict <- read.csv("Experiment_2/rps_predict_opp.csv")
colnames(rps_predict) <- c("other","self","level0","level1","level2")
rps_predict <- mutate_all(rps_predict, recode, R = "rock", P = "paper", S = "scissors")
fwg_predict <- read.csv("Experiment_2/fwg_predict_opp.csv")
colnames(fwg_predict) <- c("other","self","level0","level1","level2")
fwg_predict <- mutate_all(fwg_predict, recode, F = "fire", W = "water", G = "grass")
shootout_predict <- read.csv("Experiment_2/shootout_predict_opp.csv")
colnames(shootout_predict) <- c("other","self","level0","level1","level2")

predict_play <- function(game,self_play,other_play,level=c("level0","level1","level2")) {
  level <- match.arg(level)
  switch(as.character(game),
    rps == {
     select(filter(rps_predict,self == self_play, other == other_play),level)
    },
    fwg = {
     select(filter(fwg_predict,self == self_play, other == other_play),level)
    },
    shootout = {
     select(filter(shootout_predict,self == self_play, other == other_play),level)
    }
  )
}

dat$pred_level0 <- dat$pred_level1 <- dat$pred_level2 <- NA
for(t in 1:(nrow(dat)-1)) {
  dat$pred_level0[t+1] <- predict_play(game=dat$game[t+1],self_play = as.character(dat$human_action[t]), other_play = as.character(dat$ai_action[t]), level="level0")
  dat$pred_level1[t+1] <- predict_play(game=dat$game[t+1],self_play = dat$human_action[t], other_play = dat$ai_action[t], level="level1")
  dat$pred_level2[t+1] <- predict_play(game=dat$game[t+1],self_play = dat$human_action[t], other_play = dat$ai_action[t], level="level2")
}

Bayes_model_LL <- function(par,data) {
  alpha <- par[1] # probability that opponent plays according to strategy
  prior <- c(1,1,1) # prior alpha for dirichlet on p(level)
  prob_act <- matrix((1-alpha)*(1/3),ncol=3,nrow=nnrow(data)) # probability that opponent takes each action
  lik <- matrix(0.0,ncol=3,nrow=nnrow(data)) # likelihood of action according to three levels
  
  # level0
  prob_act[cbind(1:nrow(data),as.numeric(data$pred_level0))] <- prob_act[cbind(1:nrow(data),as.numeric(dat$pred_level0))] + alpha
  lik[,1] <- prob_act[cbind(1:nrow(data),data$ai_action)]
  
  # level1
  prob_act[cbind(1:nrow(data),as.numeric(data$pred_level1))] <- prob_act[cbind(1:nrow(data),as.numeric(dat$pred_level1))] + alpha
  lik[,2] <- prob_act[cbind(1:nrow(data),data$ai_action)]
  
  # level2
  prob_act[cbind(1:nrow(data),as.numeric(data$pred_level2))] <- prob_act[cbind(1:nrow(data),as.numeric(dat$pred_level2))] + alpha
  lik[,3] <- prob_act[cbind(1:nrow(data),data$ai_action)]
  
  cumulik <- apply(lik,2,cumprod)
    
  for(trial in 2:nrow(data)) {
    
  }
}