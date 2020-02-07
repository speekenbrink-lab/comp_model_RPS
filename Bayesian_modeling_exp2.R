library(dplyr)

dat <- read.csv("Experiment_2/dat_exp2.csv")
rps_predict <- read.csv("Experiment_2/rps_predict_opp.csv")
colnames(rps_predict) <- c("other","self","level0","level1","level2")
fwg_predict <- read.csv("Experiment_2/fwg_predict_opp.csv")
colnames(fwg_predict) <- c("other","self","level0","level1","level2")
shootout_predict <- read.csv("Experiment_2/shootout_predict_opp.csv")
colnames(shootout_predict) <- c("other","self","level0","level1","level2")

predict_play <- function(game,self_play,other_play,level=c("level0","level1","level2")) {
  level <- match.arg(level)
  switch(game,
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

Bayes_model_LL <- function(par,data) {
  alpha <- par[1]
  prior <- c(1,1,1)
  probs <- matrix(0.0,ncol=3,nrow=nnrow(data))
  probs[1,] <- 1/3
  for(trial in 2:nrow(data)) {
    pred <- 
  }
}