library(dplyr)

dat <- read.csv("Experiment_2/dat_exp2.csv")
dat <- as_tibble(dat) %>% group_by(human_id,game)
dat <- dat %>%
  mutate(ai_action_prev = lag(ai_action,1), human_action_prev = lag(human_action,1))

## the following computes probabilities of ai actions assuming
## strategy is always followed.

## level 0 predictions
dat <- dat %>% mutate(pred_a1_level0 = case_when(
  game == "rps" & ai_action_prev == "rock" ~ 1,
  game == "fwg" & ai_action_prev == "fire" ~ 1,
  game == "shootout" & ai_action_prev == "left" ~ 1,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a2_level0 = case_when(
  game == "rps" & ai_action_prev == "paper" ~ 1,
  game == "fwg" & ai_action_prev == "water" ~ 1,
  game == "shootout" & ai_action_prev == "center" ~ 1,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a3_level0 = case_when(
  game == "rps" & ai_action_prev == "scissors" ~ 1,
  game == "fwg" & ai_action_prev == "grass" ~ 1,
  game == "shootout" & ai_action_prev == "right" ~ 1,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))

## level 1 predictions
dat <- dat %>% mutate(pred_a1_level1 = case_when(
  game == "rps" & human_action_prev == "scissors" ~ 1,
  game == "fwg" & human_action_prev == "grass" ~ 1,
  game == "shootout" & human_action_prev == "left" ~ 1,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a2_level1 = case_when(
  game == "rps" & human_action_prev == "rock" ~ 1,
  game == "fwg" & human_action_prev == "fire" ~ 1,
  game == "shootout" & human_action_prev == "center" ~ 1,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a3_level1 = case_when(
  game == "rps" & human_action_prev == "paper" ~ 1,
  game == "fwg" & human_action_prev == "water" ~ 1,
  game == "shootout" & ai_action_prev == "right" ~ 1,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))

## level 2 predictions
dat <- dat %>% mutate(pred_a1_level2 = case_when(
  game == "rps" & ai_action_prev == "paper" ~ 1,
  game == "fwg" & ai_action_prev == "water" ~ 1,
  game == "shootout" & (ai_action_prev == "center" | ai_action_prev == "right") ~ .5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a2_level2 = case_when(
  game == "rps" & ai_action_prev == "scissors" ~ 1,
  game == "fwg" & ai_action_prev == "grass" ~ 1,
  game == "shootout" & (ai_action_prev == "left" | ai_action_prev == "right") ~ .5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))
dat <- dat %>% mutate(pred_a3_level2 = case_when(
  game == "rps" & ai_action_prev == "rock" ~ 1,
  game == "fwg" & ai_action_prev == "fire" ~ 1,
  game == "shootout" & (ai_action_prev == "left" | ai_action_prev == "center") ~ .5,
  is.na(ai_action_prev) ~ 1/3,
  !is.na(ai_action_prev) ~ 0))

dat$ai_action_num <- recode(dat$ai_action,"rock" = 1, "paper" = 2, "scissors" = 3, "fire" = 1, "water" = 2, "grass" = 3, "left" = 1, "center" = 2, "right" = 3)
dat$human_action_num <- recode(dat$human_action,"rock" = 1, "paper" = 2, "scissors" = 3, "fire" = 1, "water" = 2, "grass" = 3, "left" = 1, "center" = 2, "right" = 3)

# group data by subjective component id

# if human does not distinguish between players, we can run with these predictions as is
# if human does distinguish between players, we need to set the predictions to uniform at the start of a round

# if human generalizes over games, we can run update the prior generally
# if human generalizes over rounds but not games, we need to reset the prior at the start of a game
# if human does not generalize, we need to reset the prior at the start of each round

Bayes_model_LL <- function(par,data,distinct_opponent = TRUE, generalize = c("game","stage","no")) {
  # data is a subset for a single participant
  generalize <- match.arg(generalize)
  alpha <- par[1] # probability that opponent plays according to strategy
  if(length(par) > 1) {
    beta <- par[2] # probability that human plays according to best response 
  } else{
    beta <- 1 # probability that human plays according to best response 
  }
  
  prior <- c(1,1,1) # prior alpha for dirichlet on p(level)
  prior <- prior/sum(prior)
  
  # use alpha to change the "deterministic" predictions
  make_probs <- function(x) {
    (1-alpha)*(1/3) + alpha*x
  }
  dat <- ungroup(data) %>%
    mutate_at(vars(starts_with("pred_")),make_probs) 
  
  if(distinct_opponent) {
    # start of stage should not be predictable
    dat[dat$round == 1,paste0(paste0("pred_",c("a1_","a2_","a3_")),rep(c("level0","level1","level2"),each=3))] <- 1/3
    # group also by opponent
    dat <- group_by(dat,opp_type)
  }
  
  if(generalize == "stage") {
    # generalize over rounds but not game, so group by game
    dat <- group_by(dat,game,add=TRUE)
  } else if(generalize == "no") {
    # group by game and stage
    dat <- group_by(dat,game,stage,add=TRUE)
  }
  
  # compute likelihood of ai action
  dat <- dat %>%
    mutate(
      lik_level0 = case_when(
        ai_action_num == 1 ~ pred_a1_level0,
        ai_action_num == 2 ~ pred_a2_level0,
        ai_action_num == 3 ~ pred_a3_level0
      ),
      lik_level1 = case_when(
        ai_action_num == 1 ~ pred_a1_level1,
        ai_action_num == 2 ~ pred_a2_level1,
        ai_action_num == 3 ~ pred_a3_level1
      ),
      lik_level2 = case_when(
        ai_action_num == 1 ~ pred_a1_level2,
        ai_action_num == 2 ~ pred_a2_level2,
        ai_action_num == 3 ~ pred_a3_level2
      )
    )
  
  # use likelihood to compute the posterior predictive probability of each level
  dat <- dat %>%
    mutate(logpost_level0 = lag(log(prior[1]) + cumsum(log(lik_level0)),default=log(prior[1])),
           logpost_level1 = lag(log(prior[2]) + cumsum(log(lik_level1)),default=log(prior[2])),
           logpost_level2 = lag(log(prior[3]) + cumsum(log(lik_level2)),default=log(prior[3]))) %>%
    # you can add or subtract any constant from the log-likelihoods; this can aid in precision
    mutate(min = pmin(logpost_level0,logpost_level1, logpost_level2)) %>%
      mutate(normalize = exp(logpost_level0 - min) + exp(logpost_level1 - min) + exp(logpost_level2 - min)) %>%
        mutate(post_level0 = exp(logpost_level0 - min)/normalize,
               post_level1 = exp(logpost_level1 - min)/normalize,
               post_level2 = exp(logpost_level2 - min)/normalize)
  
  # use posterior predictive probability to predict probability of each ai and then human action
  dat <- dat %>%
    mutate(p_opp_a1 = post_level0*pred_a1_level0 + post_level1*pred_a1_level1 + post_level2*pred_a1_level2,
           p_opp_a2 = post_level0*pred_a2_level0 + post_level1*pred_a2_level1 + post_level2*pred_a2_level2,
           p_opp_a3 = post_level0*pred_a3_level0 + post_level1*pred_a3_level1 + post_level2*pred_a3_level2
    ) %>%
      mutate(p_self_a1 = case_when(
              game == "shootout" ~ (1-beta)*(1/3) + beta*(.5*p_opp_a2 + .5*p_opp_a3),
              TRUE ~ (1-beta)*(1/3) + beta*(p_opp_a3)
                ),
             p_self_a2 = case_when(
               game == "shootout" ~ (1-beta)*(1/3) + beta*(.5*p_opp_a1 + .5*p_opp_a3),
               TRUE ~ (1-beta)*(1/3) + beta*(p_opp_a1)
             ),
             p_self_a3 = case_when(
               game == "shootout" ~ (1-beta)*(1/3) + beta*(.5*p_opp_a1 + .5*p_opp_a2),
               TRUE ~ (1-beta)*(1/3) + beta*(p_opp_a2)
             )
        )
  
  # now finally compute the likelihood of human actions 
  dat <- dat %>% 
    mutate(
      loglik = case_when(
        human_action_num == 1 ~ log(p_self_a1),
        human_action_num == 2 ~ log(p_self_a2),
        human_action_num == 3 ~ log(p_self_a3))
    )
    
  return(-2*sum(dat$loglik))
}

library(DEoptim)

Bayes_same_game <- Bayes_same_stage <- Bayes_same_no <- 
  Bayes_distinct_game <- Bayes_distinct_stage <- Bayes_distinct_no <- 
    list()

for(id in levels(dat$human_id)) {
  tdat <- subset(dat,human_id == id)
  ctrl <- DEoptim.control(NP = 20, itermax=50)
  Bayes_same_game[[id]] <- DEoptim(Bayes_model_LL, lower=c(0), upper = c(1), data = tdat,
                                   distinct_opponent = FALSE, generalize = "game",control=ctrl)
  Bayes_same_stage[[id]] <- DEoptim(Bayes_model_LL, lower=c(0), upper = c(1), data = tdat,
                                   distinct_opponent = FALSE, generalize = "stage",control=ctrl)
  Bayes_same_no[[id]] <- DEoptim(Bayes_model_LL, lower=c(0), upper = c(1), data = tdat,
                                   distinct_opponent = FALSE, generalize = "no",control=ctrl)
  Bayes_distinct_game[[id]] <- DEoptim(Bayes_model_LL, lower=c(0), upper = c(1), data = tdat,
                                   distinct_opponent = TRUE, generalize = "game",control=ctrl)
  Bayes_distinct_stage[[id]] <- DEoptim(Bayes_model_LL, lower=c(0,0), upper = c(1,1), data = tdat,
                                        distinct_opponent = TRUE, generalize = "stage",control=ctrl)
  Bayes_distinct_no[[id]] <- DEoptim(Bayes_model_LL, lower=c(0), upper = c(1), data = tdat,
                                 distinct_opponent = TRUE, generalize = "no",control=ctrl)
}



# this will implement a pseudo-multinomial-dirichlet
pBayes_model_LL <- function(par,data,distinct_opponent = TRUE, generalize = c("game","stage","no")) {
  generalize <- match.arg(generalize)
  alpha <- par[1] # probability that opponent plays according to strategy
  if(length(par) > 1) {
    gamma <- par[2] # probability that human plays according to best response 
  } else{
    gamma <- 1 # probability that human plays according to best response 
  }
  if(length(par) > 2) {
    beta <- par[3] # probability that human plays according to best response 
  } else{
    beta <- 1 # probability that human plays according to best response 
  }
  
  prior <- c(gamma,gamma,gamma) # prior alpha for dirichlet on p(level)
  #prior <- prior/sum(prior)
  
  # use alpha to change the "deterministic" predictions
  make_probs <- function(x) {
    (1-alpha)*(1/3) + alpha*x
  }
  dat <- ungroup(data) %>%
    mutate_at(vars(starts_with("pred_")),make_probs) 
  
  if(distinct_opponent) {
    # start of round should not be predictable
    dat[dat$round == 1,paste0(paste0("pred_",c("a1_","a2_","a3_")),rep(c("level0","level1","level2"),each=3))] <- 1/3
    # group also by opponent
    dat <- group_by(dat,opp_type)
  }
  
  if(generalize == "stage") {
    # generalize over rounds but not game, so group by game
    dat <- group_by(dat,game,add=TRUE)
  } else if(generalize == "no") {
    # group by game and stage
    dat <- group_by(dat,game,stage,add=TRUE)
  }
  
  # compute likelihood of ai action
  dat <- dat %>%
    mutate(
      lik_level0 = case_when(
        ai_action_num == 1 ~ pred_a1_level0,
        ai_action_num == 2 ~ pred_a2_level0,
        ai_action_num == 3 ~ pred_a3_level0
      ),
      lik_level1 = case_when(
        ai_action_num == 1 ~ pred_a1_level1,
        ai_action_num == 2 ~ pred_a2_level1,
        ai_action_num == 3 ~ pred_a3_level1
      ),
      lik_level2 = case_when(
        ai_action_num == 1 ~ pred_a1_level2,
        ai_action_num == 2 ~ pred_a2_level2,
        ai_action_num == 3 ~ pred_a3_level2
      )
    )
  
  # normalize the likeihoods to give a pseudo-observation of strategy
  dat <- dat %>%
    # compute normalizing factor
    mutate(normalize = lik_level0 + lik_level1 + lik_level2) %>%
    # avoid division by 0
    mutate(normalize = replace(normalize, normalize == 0, 1)) %>%
    # normalize
    mutate(pseudo_obs_level0 = lik_level0/normalize,
           pseudo_obs_level1 = lik_level1/normalize,
           pseudo_obs_level2 = lik_level2/normalize)
  
  # use pseudo observations to compute the posterior predictive probability of each level
  dat <- dat %>%
    mutate(post_level0 = lag(prior[1] + cumsum(pseudo_obs_level0),default=prior[1]),
           post_level1 = lag(prior[2] + cumsum(pseudo_obs_level1),default=prior[2]),
           post_level2 = lag(prior[3] + cumsum(pseudo_obs_level2),default=prior[3])) %>%
      mutate(normalize = post_level0 + post_level1 + post_level2) %>%
        mutate(post_level0 = post_level0/normalize,
               post_level1 = post_level1/normalize,
               post_level2 = post_level2/normalize)
  
  # use posterior predictive probability to predict probability of each ai and then human action
  dat <- dat %>%
    mutate(p_opp_a1 = post_level0*pred_a1_level0 + post_level1*pred_a1_level1 + post_level2*pred_a1_level2,
           p_opp_a2 = post_level0*pred_a2_level0 + post_level1*pred_a2_level1 + post_level2*pred_a2_level2,
           p_opp_a3 = post_level0*pred_a3_level0 + post_level1*pred_a3_level1 + post_level2*pred_a3_level2
    ) %>%
    mutate(p_self_a1 = case_when(
      game == "shootout" ~ (1-beta)*(1/3) + beta*(.5*p_opp_a2 + .5*p_opp_a3),
      TRUE ~ (1-beta)*(1/3) + beta*(p_opp_a3)
    ),
    p_self_a2 = case_when(
      game == "shootout" ~ (1-beta)*(1/3) + beta*(.5*p_opp_a1 + .5*p_opp_a3),
      TRUE ~ (1-beta)*(1/3) + beta*(p_opp_a1)
    ),
    p_self_a3 = case_when(
      game == "shootout" ~ (1-beta)*(1/3) + beta*(.5*p_opp_a1 + .5*p_opp_a2),
      TRUE ~ (1-beta)*(1/3) + beta*(p_opp_a2)
    )
    )
  
  # now finally compute the likelihood of human actions 
  dat <- dat %>% 
    mutate(
      loglik = case_when(
        human_action_num == 1 ~ log(p_self_a1),
        human_action_num == 2 ~ log(p_self_a2),
        human_action_num == 3 ~ log(p_self_a3))
    )
  
  return(-2*sum(dat$loglik))
}

pBayes_same_game <- pBayes_same_stage <- pBayes_same_no <- 
  pBayes_distinct_game <- pBayes_distinct_stage <- pBayes_distinct_no <- 
  list()

for(id in levels(dat$human_id)) {
  tdat <- subset(dat,human_id == id)
  ctrl <- DEoptim.control(NP = 20, itermax=50)
  pBayes_same_game[[id]] <- DEoptim(pBayes_model_LL, lower=c(0.1,1), upper = c(1,100), data = tdat,
                                   distinct_opponent = FALSE, generalize = "game",control=ctrl)
  pBayes_same_stage[[id]] <- DEoptim(pBayes_model_LL, lower=c(0.1,1), upper = c(1,100), data = tdat,
                                    distinct_opponent = FALSE, generalize = "stage",control=ctrl)
  pBayes_same_no[[id]] <- DEoptim(pBayes_model_LL, lower=c(0.1,1), upper = c(1,100), data = tdat,
                                 distinct_opponent = FALSE, generalize = "no",control=ctrl)
  pBayes_distinct_game[[id]] <- DEoptim(pBayes_model_LL, lower=c(0.1,1), upper = c(1,100), data = tdat,
                                       distinct_opponent = TRUE, generalize = "game",control=ctrl)
  pBayes_distinct_stage[[id]] <- DEoptim(pBayes_model_LL, lower=c(0.1,1), upper = c(1,100), data = tdat,
                                        distinct_opponent = TRUE, generalize = "stage",control=ctrl)
  pBayes_distinct_no[[id]] <- DEoptim(pBayes_model_LL, lower=c(0.1,1), upper = c(1,100), data = tdat,
                                     distinct_opponent = TRUE, generalize = "no",control=ctrl)
}
