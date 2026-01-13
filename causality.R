library(tidyverse)
library(lmtp)
source("globals.R")

default_baseline <-
  c(
    "gender",
    "age",
    "PAC.general",
    "TPI.extraversion",
    "TPI.agreeableness",
    "TPI.conscientiousness",
    "TPI.emotional_stability",
    "TPI.openness_to_experiences"
    
  )
all_outcomes <- c(wb_vars, "JAJ.ability", "DER.score", "GDS.urge_to_dance")
filter_gender <- function(data){
  data %>% filter(gender %in% c("female", "male"))
}

select_numeric <- function(data){
  data %>% select(where(is.numeric))
}

select_character <- function(data){
  data %>% select(where(is.character))
}

char_to_factor <- function(data){
  data %>% mutate(across(where(is.character), as.factor))
}

to_percent <- function(tab, col){
  prop.table(tab, col) %>% round(3) %>% (function(x) x*100)
}

bulk_scale <- function(data, vars = NULL){
  if(is.null(vars) || all(is.na(vars))){
    vars <- names(select_numeric(data))
  }
  data %>% mutate(across(all_of(vars), function(x) scale(x) %>% as.numeric()))
}

add_total_positive <- function(data = master, 
                               vars = c(wb_vars, cog_vars,  "HOP.general", "SWL.general") ){
  #browser()
  tmp <- data %>% 
    select(p_id, time_point, all_of(unique(vars))) %>% 
    bulk_scale() %>% 
    pivot_longer(-c(p_id, time_point)) %>% 
    group_by(p_id, time_point) %>% 
    mutate(total_positive = get_polarity(name) *  value) %>% 
    ungroup()  
  
  tmp <- tmp %>%   
    group_by(p_id, time_point) %>% 
    summarize(total_positive = sum(total_positive, na.rm = T), .groups = "drop") %>% 
    mutate(total_positive = scale(total_positive) %>% as.numeric())
  
  data %>% 
    left_join(tmp %>% select(p_id, time_point, total_positive))
}

factor_to_num <- function(fac, start = 0){
  as.integer(as.factor(fac)) - (1 - start)
}

causal_effect <- function(data = master_red, 
                          outcome = all_outcomes, 
                          baseline = default_baseline, 
                          trt = "p_group", 
                          differential = F,
                          fast = T){
  
  d1 <- function(data, trt){
    rep("dance", nrow(data))
  }
  
  d0 <- function(data, trt){
    rep("control", nrow(data))
  }
  
  lmtp_data <- data %>% add_total_positive() %>% 
    filter(time_point == "T1") %>% 
    select(p_id, p_group, all_of(c(outcome, baseline))) %>% na.omit()
  
  if(differential){
    #browser()
    lmtp_data <- dt_var(data %>% add_total_positive(), 
                        time_points = c("T1","T4"), 
                        vars = outcome, rel_change = T,
                        covariates = c(baseline, "p_group")) %>% 
      na.omit()
  }
    
  print(nrow(lmtp_data))
  print(table(lmtp_data$p_group))
  map_dfr(outcome, function(oc){
    treat <- lmtp_tmle(data = lmtp_data, 
                       trt = trt, 
                       outcome = oc,
                       baseline = baseline, 
                       outcome_type = "continuous", 
                       shift = d1, 
                       folds = 1, 
                       learners_trt = "SL.glm", 
                       learners_outcome = "SL.glm")
    control <- lmtp_tmle(data = lmtp_data, 
                         trt = trt, 
                         outcome = oc,
                         baseline = baseline, 
                         outcome_type = "continuous", 
                         shift = d0, 
                         folds = 1, 
                         learners_trt = "SL.glm", 
                         learners_outcome = "SL.glm")
    #browser()
    if(oc == "SWB.anxiety"){
      #browser()
    }
    #browser()
    ATE_tmle_tmle <-NULL
    if(!fast){
      fit_tmle_tmle <-  tmle::tmle(Y = lmtp_data[[oc]], 
                                   A = factor_to_num(lmtp_data[[trt]]), 
                                   W = lmtp_data[,baseline])
      ATE_tmle_tmle <- tibble(shift = fit_tmle_tmle$estimates$EY1$psi,
                              ref = fit_tmle_tmle$estimates$EY0$psi,
                              estimate = fit_tmle_tmle$estimates$ATE$psi,
                              p.value = fit_tmle_tmle$estimates$ATE$pvalue,
                              conf.low = fit_tmle_tmle$estimates$ATE$CI[1],
                              conf.high = fit_tmle_tmle$estimates$ATE$CI[2],
                              std.error = .5*(conf.high - conf.low)/1.96)
      
    }
                            
    
    fit <- lmtp_data  %>% select(all_of(c(baseline, oc, trt))) %>% 
      mutate(p_group = factor(p_group, levels = c("control", "dance"))) %>% 
      glm(as.formula(sprintf("%s ~ .", oc)), data =.)
    
    raw_diff <- lmtp_data %>% 
      group_by(p_group) %>% 
      summarise(m = mean(!!sym(oc)), 
                se = sd(!!sym(oc))/sqrt(n()), .groups = "drop")
    raw_d <- raw_diff[raw_diff$p_group == "dance",]$m - raw_diff[raw_diff$p_group == "control",]$m
    raw_se <- sqrt(sum(raw_diff$se^2))
    
    ATE_raw <- raw_diff$m %>% 
      t() %>% 
      as_tibble() %>% 
      set_names(c("shift", "ref")) %>% 
      mutate(estimate = raw_d, 
             std.error = raw_se, 
             conf.low = estimate - 1.96*raw_se, 
             conf.high = estimate+  1.96*raw_se, 
             p.value = lmtp_data %>% rstatix::t_test(as.formula(sprintf("%s ~ p_group", oc))) %>% pluck("p"))
    
    #browser()
    ATE_naive <- marginaleffects::avg_comparisons(fit, variables = trt, vcov = T) %>% 
      as_tibble() %>% 
      select(shift = predicted_lo, 
             ref = predicted_hi,
             estimate, 
             std.error, 
             p.value, 
             conf.low, 
             conf.high, 
             ) #%>% mutate(estimate =  -estimate, conf.low = - conf.low, conf.high = -conf.high ) 
    ATE_tmle <- lmtp_contrast(treat, ref = control) %>% pluck("estimates") 
    bind_rows(ATE_raw %>% mutate(type = "RAW"),
              ATE_naive %>% mutate(type = "NAIVE"), 
              ATE_tmle %>% mutate(type = "LMPT_TMLE"),
              if(!fast)ATE_tmle_tmle %>% mutate(type = "TMLE_TMLE") ) %>% 
      mutate(outcome = oc) %>% select(outcome, everything())
  })
}
