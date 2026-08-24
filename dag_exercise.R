library(tidyverse)
library(dagitty)
library(ggdag)
library(ggplot2)
library(MatchIt)
library(ipw)
library(marginaleffects)
library(lmtp)

backdoor_example <- function(){
  example_dag <- dagitty('dag{
  Y <- A -> X
  B <- X -> Y -> C
  Y <- B -> C
  }')
  ggdag(example_dag) + theme_dag() + geom_dag_node(color = "white") +geom_dag_text(color = "black", size = 10) 
  adjustmentSets(example_dag, exposure = "X", outcome =  "Y")
  adjustmentSets(example_dag, exposure = "X", outcome =  "C")
}

# Define the DAG using dagitty syntax
longgold_dag_example <- function(){
  music_dag_v2 <- dagitty('dag {
    SES.educational_degree -> MHE.general_score
    SES.educational_degree -> MIQ.score
    SES.educational_degree -> GMS.musical_training
    gender -> CCM 
    gender -> TOM.improvement 

    TOI.incremental_theory -> TOM.improvement
    TOI.incremental_theory -> GRT
    TOM.improvement -> GRT
    MHE.general_score -> GMS.active_engagement
    MHE.general_score -> GMS.musical_training
    MHE.general_score -> SEM.behavioral_engagement

    GRT -> SEM.behavioral_engagement
    GRT -> GMS.musical_training

    GMS.musical_training -> CCM
    GMS.musical_training -> GMS.active_engagement
    GMS.musical_training -> MDI.score 
    CCM -> MDI.score 

    GMS.active_engagement <- GMS.musical_training
}')
  

  ggdag(music_dag_v2) + theme_dag() + 
    geom_dag_node(color = "green")+
    geom_dag_text(color = "black", size = 4) +
    labs(title = "Corrected Causal Model (Parental SES Exogenous)")
  
}

simple_dag_step1 <- dagitty('dag{
t -> y
}')

simple_dag_step2 <- dagitty('dag{
l2 -> y 
t -> y
l1 -> y
}')

simple_dag_ext <- dagitty('dag{
l1 -> y 
t -> y
l1 -> t
l2 -> t
a -> y
t -> y
t -> a
l1 -> y
l1 -> z <- y
l2 -> z
}')

dancer_health_dag <- dagitty('dag{
age -> PHQ 
age -> engagement_level -> PHQ
age -> PHQ 
age -> social_contact -> PHQ
age -> MHC 
age -> engagement_level -> MHC
age -> MHC 
age -> social_contact -> MHC

sex -> PHQ 
sex -> engagement_level -> PHQ
sex -> PHQ 
sex -> social_contact -> PHQ
sex -> MHC 
sex -> engagement_level -> MHC
sex -> MHC 
sex -> social_contact -> MHC
SES -> PHQ 
SES -> "engagement_level" -> PHQ
SES -> PHQ 
SES -> social_contact -> PHQ
SES -> MHC 
SES -> engagement_level -> MHC
SES -> MHC 
SES -> social_contact -> MHC
age -> SES
sex -> SES
PHQ -> MHC
}')
dancer_health_dag_PHQ <- dagitty('dag{
age -> PHQ 
age -> engagement_level -> PHQ
age -> PHQ 
age -> social_contact -> PHQ

sex -> PHQ 
sex -> engagement_level -> PHQ
sex -> PHQ 
sex -> social_contact -> PHQ

SES -> PHQ 
SES -> "engagement_level" -> PHQ
SES -> PHQ 
SES -> social_contact -> PHQ
age -> SES
sex -> SES
}')

simple_tmle <- function(data, trt = "t", baseline = c("l1", "l2"), outcome = "y", outcome_type = "continuous"){
  mod1 <- lmtp_tmle(data = data, 
                    trt = trt, 
                    baseline = baseline, 
                    outcome = outcome, 
                    outcome_type = outcome_type, 
                    shift = static_binary_on)
  mod0 <- lmtp_tmle(data = data, 
                    trt = trt, 
                    baseline = baseline, 
                    outcome = outcome, 
                    outcome_type = outcome_type, 
                    shift = static_binary_off)
  lmtp_contrast(mod1, ref = mod0)
  
}
simulate_simple_example <- function(n = 1000, 
                                    seed = global_seed, 
                                    threshold = .1, 
                                    cov_rel = 0){
  set.seed(seed)
  simdat <- tibble(l1 = rnorm(n, 10, 5))
  
  #use some normal distribution to defined treatment
  a.lin <- simdat$l1 - 10
  b.lin <- rnorm(n, 2, 5)
  
  #c.lin will be unobserved
  c.lin <- rnorm(n, 1, 5)
  
  #logistic experssion to defone probality to be treated 
  pa <- exp(a.lin + b.lin + c.lin)/(1 + exp(a.lin + b.lin + c.lin))
  
  #simulate binary treatment based on probability to treat, which in turn is based on 2 obversed and on unobserved covariate
  simdat$t <- rbinom(n, 1, prob = pa)
  
  #add reandom treatment for whatever reasons
  simdat$r <- sample(c(-1,1), nrow(simdat), replace = T)
  
  #outcome
  simdat$y <- 10 * simdat$t + 2 * simdat$l1 + rnorm(n, -10, 5)
  
  #Covariate l1 with measurement error based on cov_rel (covariate reliability)
  simdat$l1_me <- simdat$l1 + rnorm(n, 0, cov_rel) 
  
  #covariate l2
  simdat$l2 <- b.lin  

  #Covariate l2 with measurement error, based on cov_rel)
  simdat$l2_me <- b.lin + rnorm(n, 0, cov_rel) 
  
  #change threshold/2 percents of treatments  
  shuffle_idx <- which(runif(nrow(simdat)) > (1 - threshold))
  simdat$t_me <- simdat$t
  simdat$t_me[shuffle_idx] <- sample(c(0, 1), length(shuffle_idx), replace = T)
  
  #add a collider, for education purposes
  simdat <- simdat %>% mutate(z = scale((y + l1 * l2))[,1] + rnorm(nrow(simdat), 10, 1))
  #add ids for completeness
  simdat$id <- 1:nrow(simdat)
  simdat
}


prepare_fit_data <- function(fit, trt = "t"){
  #browser()
  ret <- fit %>% 
    broom::tidy() %>%
    filter(term == trt) %>% 
    select(-term)
  
  fit$model %>% 
    group_by(!!sym(trt)) %>% 
    summarise(m = mean(y)) %>% 
    pivot_wider(names_from = !!sym(trt), 
                values_from = m) %>% 
    set_names(c("ref", "shift")) %>% 
    bind_cols(ret)
}

comp_ATE_simple_dags <- function(with_measurement_error = F, 
                                 data = NULL){
  if(is.null(data)){
    if(!with_measurement_error){
      simple <- simulate_simple_example()
    }
    else{
      simple <- simulate_simple_example(cov_rel = 5)
      simple <- simple %>% mutate(l1 = l1_me, l2 = l2_me)
    }
  } 
  else{
    if(!with_measurement_error){
      simple <- data
    }
    else{
      simple <- simple %>% mutate(l1 = l1_me, l2 = l2_me)
    }

  }  
  fit0 <- simple %>% lm(y ~ t, data =.)
  fit0_a <- simple %>% lm(y ~ t_me, data =.)
  fit1 <- simple %>% lm(y ~ t + l1 + l2, data =.)
  fit1_a <- simple %>% lm(y ~ t_me + l1 + l2, data =.)
  fit1_z_cov <- simple %>% lm(y ~ t + l1 + l2 + z, data =.)
  fit1_z <- simple %>% lm(y ~ t + z + l2, data =.)
  ret <- bind_rows(
    prepare_fit_data(fit0) %>% mutate(method = "none", spec = "no covariates", trt = "clean"),
    prepare_fit_data(fit0_a, "t_me") %>% mutate(method = "none", spec = "no covariates", trt ="noisy"),
    prepare_fit_data(fit1) %>% mutate(method = "none", spec = "covariates", trt = "clean"),
    prepare_fit_data(fit1_a, "t_me") %>% mutate(method = "none", spec = "covariates", trt = "noisy"),
    prepare_fit_data(fit1_z) %>% mutate(method = "none", spec = "wrong covariates", trt = "clean"),
    prepare_fit_data(fit1_z_cov) %>% mutate(method = "none", spec = "covariates + collider", trt = "clean"),
  ) %>% mutate(weights = "none")
  tmle <- simple_tmle(simple)
  tmle_a <- simple_tmle(simple, trt = "t_me", baseline = c("l1", "l2"))
  tmle_z <- simple_tmle(simple, baseline = c("z", "l2"))
  tmle_a_z <- simple_tmle(simple, trt = "t_me", baseline = c("l1", "l2", "z"))
  ret <- ret %>% bind_rows(
    tmle$estimates %>% mutate(method = "tmle" , spec = "covariates", trt = "clean")%>% mutate(weights = "tmle"),
    tmle_a$estimates %>% mutate(method = "tmle" , spec = "covariates", trt = "noisy")%>% mutate(weights = "tmle"),
    tmle_z$estimates %>% mutate(method = "tmle" , spec = "wrong covariates", trt = "clean")%>% mutate(weights = "tmle"),
    tmle_a_z$estimates %>% mutate(method = "tmle" , spec = "covariates + collider", trt = "noisy")%>% mutate(weights = "tmle")
  ) 
  match_full <- simple %>% 
    MatchIt::matchit(t ~ l1 + l2, method = "full", 
                     distance = "glm", 
                     link = "logit", data  =.) 
  match_full_z <- simple %>% MatchIt::matchit(t ~ z + l2, 
                                   method = "full", 
                                   distance = "glm", 
                                   link = "logit", 
                                   data  = .) 
  simple_full <- match_data(match_full)
  simple_full_z <- match_data(match_full_z)
  match_cem <- simple %>% 
    MatchIt::matchit(t ~ l1 + l2, 
                     method = "cem", data  =.)
  match_cem_z <- simple %>% 
    MatchIt::matchit(t ~ z + l2, 
                     method = "cem", data  =.)
  simple_cem <- match_data(match_cem)
  simple_cem_z <- match_data(match_cem_z)
  
  fit_full1 <- simple_full %>% 
    lm(y ~ t + l1 + l2, 
       data = .)
  
  fit_full1_w <- simple_full %>% 
    lm(y ~ t + l1 + l2, 
       weights = weights, 
       data = .)
  
  fit_full0_w <- simple_full %>% 
    lm(y ~ t, 
       weights = weights, 
       data = .)
  
  fit_full1_z <- simple_full_z %>% 
    lm(y ~ t + z + l2, 
       data = .)
  
  fit_full1_z_w <- simple_full %>% 
    lm(y ~ t + z + l2, 
       weights = weights, 
       data = .)
  
  fit_full1_coll <- simple_full_z %>% 
    lm(y ~ t + l1 + l2 + z, 
       data = .)
  
  ret <- ret %>% bind_rows(
    prepare_fit_data(fit_full1) %>% mutate(method = "full matching", spec ="covariates", weights = "none", trt = "clean"),
    prepare_fit_data(fit_full1_w) %>% mutate(method = "full matching", spec = "covariates", weights = "IPW", trt = "clean"),
    prepare_fit_data(fit_full0_w) %>% mutate(method = "full matching", spec = "no covariates", weights = "IPW", trt = "clean"),
    prepare_fit_data(fit_full1_z) %>% mutate(method = "full matching", spec = "wrong covariates", weights = "none", trt = "clean"),
    prepare_fit_data(fit_full1_coll) %>% mutate(method = "full matching", spec = "covariates + collider", weights = "none", trt = "clean")
  ) 
    
  fit_cem0 <- simple_cem %>% 
    lm(y ~ t, 
       weights = weights, 
       data = .)
  
  fit_cem1 <- simple_cem %>% 
    lm(y ~ t + l1 + l2, 
       data = .)
  
  fit_cem1_w <- simple_cem %>% 
    lm(y ~ t + l1 + l2, 
       weights = weights, 
       data = .)
  
  fit_cem1_z <- simple_cem %>% 
    lm(y ~ t + z + l2, 
       data = .)
  
  fit_cem1_z_w <- simple_cem_z %>% 
    lm(y ~ t + z + l2, 
       weights = weights, 
       data = .)
  
  ret <- ret %>% bind_rows(
    prepare_fit_data(fit_cem1) %>% mutate(method  = "CEM matching", spec  = "covariates", weights ="none", trt = "clean"),
    prepare_fit_data(fit_cem1_w) %>% mutate(method = "CEM matching", spec  = "covariates", weights ="IPW", trt = "clean"),
    prepare_fit_data(fit_cem1_w) %>% mutate(method = "CEM matching", spec  = "no covariates", weights = "IPW", trt = "clean"),
    prepare_fit_data(fit_cem1_z) %>% mutate(method = "CEM matching", spec  = "wrong covariates", weights = "none", trt = "clean"),
    prepare_fit_data(fit_cem1_z_w) %>% mutate(method = "CEM matching", spec  = "wrong covariates", weights = "IPW", trt = "clean")
  ) 
  ret %>% 
    mutate(with_measurement_error = c("none", "some")[as.integer(with_measurement_error) + 1]) 
}
