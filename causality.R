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

setup_workspace <- function(){
  master <- readRDS("data/flying_steps_data.rds")
  master_red <- master %>% filter_gender()
  nhefs <- haven::read_sas("data/nhefs.sas7bdat")
  
  nhefs <- nhefs %>% 
    mutate(
      # add id and censored indicator
      id = 1:n(),
      censored = ifelse(is.na(wt82), 1, 0),
      # recode age > 50 and years of school to categories
      older = case_when(
        is.na(age) ~ NA_real_,
        age > 50 ~ 1,
        TRUE ~ 0
      ),
      education = case_when(
        school <  9 ~ 1,
        school <  12 ~ 2,
        school == 12 ~ 3,
        school < 16 ~ 4,
        TRUE ~ 5
      )
    ) %>% 
    #  change categorical variables to factors
    mutate_at(vars(sex, race, education, exercise, active), factor) 
  nhefs_complete <- nhefs %>% 
    drop_na(qsmk, sex, race, age, school, smokeintensity, smokeyrs, exercise, active, wt71, wt82, wt82_71, censored)
  assign("foo", simulate_class_trick(seed = global_seed), globalenv())
  assign("sim20", simulate_20_1(l1_effect = 0, mult = 1, seed = global_seed), globalenv())
  assign("sim20_l1", simulate_20_1(l1_effect = -10, mult = 1, seed = global_seed), globalenv())
  
  assign("nhefs_complete", nhefs_complete, globalenv())
  assign("master", master, globalenv())
  assign("master_red", master_red, globalenv())
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
                            
    #alt_form <- sprintf("%s ~ (%s) * %s", oc, paste(baseline, collapse = " + "), trt)
    fit <- lmtp_data  %>% select(all_of(c(baseline, oc, trt))) %>% 
      mutate(p_group = factor(p_group, levels = c("control", "dance"))) %>% 
      glm(as.formula(sprintf("%s ~ .", oc)), data =.)
    #browser()
    fit2 <- lmtp_data  %>% select(all_of(c(baseline, oc, trt))) %>% 
      mutate(p_group = factor_to_num(p_group)) %>% 
      glm(as.formula(sprintf("%s ~ .", oc)), data =.)
    
    ATE_std_reg_est <- stdReg::stdGlm(fit2, data = lmtp_data, X = "p_group", x = c(0, 1)) %>% 
      summary() %>% 
      pluck("est.table") %>% 
      janitor::clean_names() %>% 
      as_tibble()
    
    ATE_std_reg_d <- diff(ATE_std_reg_est$estimate)
    ATE_std_reg_se <- sqrt(sum(ATE_std_reg_est$std_error^2))
    ATE_std_reg_p <-  2 * pt(abs(ATE_std_reg_d / ATE_std_reg_se), nrow(lmtp_data) -1, lower.tail = FALSE)
    
    ATE_std_reg <- 
      ATE_std_reg_est$estimate %>% 
      t() %>% 
      as_tibble() %>% 
      set_names(c("ref", "shift")) %>% 
      mutate(estimate = ATE_std_reg_d, 
             std.error = ATE_std_reg_se, 
             conf.low = estimate - 1.96*std.error, 
             conf.high = estimate+  1.96*std.error, 
             p.value = ATE_std_reg_p)
    
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
    
    browser()
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
              ATE_std_reg %>% mutate(type = "STDREG"),
              if(!fast)ATE_tmle_tmle %>% mutate(type = "TMLE_TMLE") ) %>% 
      mutate(outcome = oc) %>% select(outcome, everything())
  })
}
