global_seed <- 786543

simulate_class_trick  <- function(n = 1000, seed = NULL) {
  set.seed(seed)
  l0 <- rnorm(n)
  a1 <- rbinom(n, 1, plogis(-1 + l0 * 1.5))
  l1 <- rnorm(n)
  a2 <- rbinom(n, 1, plogis(-1 + l1 * 1.5 + a1 * 2))
  y <- rnorm(n, -1 - 1.2 * l0 + 2.4 * a1 - 2 * l1 + 1.2 * a2)
  tibble(
    L1 = l0,
    A1 = a1,
    L2 = l1,
    A2 = a2,
    Y = y
  )
}

d_foo <- function(a) {
  epsilon <- runif(length(a))
  ifelse(epsilon < 0.5 & a == 1, 0, a)
}

get_density_ratios <- function(data = foo, seed = 786543) {
  #browser()
  set.seed(seed)
  # Define the intervention as a function
  # Create an empty matrix to store the density ratios
  density_ratios <- matrix(NA, nrow = nrow(data), ncol = 2)
  
  for (t in 1:2) {
    a <- c("A1", "A2")[t]
    # Duplicate the data
    shifted <- data
    # In the duplicated data, modify treatment according to d
    shifted[[a]] <- d(data[[a]])
    
    # Set the values of delta in the original and modified data
    data$lambda <- 0
    shifted$lambda <- 1
    
    # Stack the original data and the modified data
    density_ratio_data <- bind_rows(data, shifted)
    
    # Regress lambda_t on A_t,H_t
    parents <- unlist(lapply(1:t, \(x) paste0(c("L", "A"), x)))
    formula <- as.formula(paste("lambda ~", paste(parents, collapse = "+")))
    density_ratio_model <- glm(formula, data = density_ratio_data, family = binomial)
    
    # Calculate the estimated odds
    prob_lambda_1 <- predict(density_ratio_model, data, type = "response")
    density_ratios[, t] <- prob_lambda_1 / (1 - prob_lambda_1)
  }
  density_ratios
}
tmle_man <- function(data = foo, d = d_foo) {
  
  browser()
  density_ratios <- get_density_ratios(data)
  # First compute the weights as the cumulative product of the density ratios
  weights <- t(apply(density_ratios, 1, cumprod))
  
  predictions_natural <- matrix(NA, nrow = nrow(data), ncol = 2)
  predictions_shifted <- matrix(NA, nrow = nrow(data), ncol = 2)
  
  # Set t = 2
  # Because t + 1 = tau + 1, set the first pseudo outcome to the observed outcome
  m3_d <- data$Y
  
  # Regress the pseudo outcome on the observed data
  fit2 <- glm(m3_d ~ A2 + L2 + A1 + L1, data = data)
  
  # Generate the predictions for t = 2
  predictions_natural[, 2] <- predict(fit2)
  predictions_shifted[, 2] <- predict(fit2, mutate(data, A2 = d(A2)))
  
  # Perform the targeting step
  targeting_fit2 <- glm(m3_d ~ offset(predictions_natural[, 2]), weights = weights[, 2])
  
  # Update predictions
  predictions_natural[, 2] <- predictions_natural[, 2] + coef(targeting_fit2)
  predictions_shifted[, 2] <- predictions_shifted[, 2] + coef(targeting_fit2)
  
  # Iterate, setting t - 1 = 1
  # The new pseudo outcome is the targeted outcome under the shift at t + 1 = 2
  m2_d <- predictions_shifted[, 2]
  
  # Regress the pseudo outcome on the observed data
  fit1 <- glm(m2_d ~ A1 + L1, data = data)
  
  # Generate the predictions for t = 2
  predictions_natural[, 1] <- predict(fit1)
  predictions_shifted[, 1] <- predict(fit1, mutate(data, A1 = d(A1)))
  
  # Perform the targeting step
  targeting_fit1 <- glm(m2_d ~ offset(predictions_natural[, 1]), weights = weights[, 1])
  
  # Update predictions
  predictions_natural[, 1] <- predictions_natural[, 1] + coef(targeting_fit1)
  predictions_shifted[, 1] <- predictions_shifted[, 1] + coef(targeting_fit1)
  
  mean(predictions_shifted[, 1])
}

sdr_man <- function(data = foo, d = d_foo){
  
  browser()
  density_ratios <- get_density_ratios(data)
  # First compute the weights as the cumulative product of the density ratios

  predictions_natural <- matrix(NA, nrow = nrow(data), ncol = 2)
  predictions_shifted <- matrix(NA, nrow = nrow(data), ncol = 3)

  # Set t = 2
  # Because t + 1 = tau + 1, set the first pseudo outcome to the observed outcome
  m3_d <- data$Y
  predictions_shifted[, 3] <- m3_d
  
  # Regress the pseudo outcome on the observed data
  fit2 <- glm(m3_d ~ A2 + L2 + A1 + L1, data = data)
  
  # Generate the predictions for t = 2
  predictions_natural[, 2] <- predict(fit2)
  predictions_shifted[, 2] <- predict(fit2, mutate(data, A2 = d(A2)))
  
  # Iterate, setting t - 1 = 1
  # Compute the pseudo outcome using the efficient influence function
  m2_d <- density_ratios[, 2]*(m3_d - predictions_natural[, 2]) + predictions_shifted[, 2]
  
  # Regress the pseudo outcome on the observed data
  fit1 <- glm(m2_d ~ A1 + L1, data = data)
  
  # Generate the predictions for t = 1
  predictions_natural[, 1] <- predict(fit1)
  predictions_shifted[, 1] <- predict(fit1, mutate(data, A1 = d(A1)))
  
  weights <- t(apply(density_ratios, 1, cumprod))
  
  # Compute the estimate as the sample mean of the uncentered efficient influence function
  uc_eif <- rowSums(weights * (predictions_shifted[, 2:3] - predictions_natural[, 1:2])) + 
    predictions_shifted[, 1]
  
  mean(uc_eif)
}
simulate_20_1 <- function(n, mult = 1, sd = 10, l1_effect = 0, sd_l1 = 1, seed = NULL){
  set.seed(seed)
  spec <-
    tribble(~N, ~A0, ~L1, ~A1,~Y,
          24, 0, 0, 0, 84,
          16, 0, 0, 1, 84,
          24, 0, 1, 0, 52,
          96, 0, 1, 1, 52,
          48, 1, 0, 0, 76,
          32, 1, 0, 1, 76,
          16, 1, 1, 0, 44,
          64, 1, 1, 1, 44)
  ret <- 
    map_dfr(1:nrow(spec), function(i){
    tibble( 
           Y = rnorm(spec$N[i] * mult, mean = spec$Y[i], sd = sd),    
           A0 = spec$A0[i], 
           L1 = spec$L1[i], 
           A1 = spec$A1[i])
  }) %>% mutate(p_id = 1:nrow(.))   
  ret %>% mutate(Y2 = pmax(0, Y + rnorm(nrow(.), (A1 + L1) * l1_effect, sd_l1))) 
}
expit <- function(x){
  #exp(x)/(1 + exp(x))
  1/(1 + exp(-x))
}
simulate_kang_schafer <- function(n = 200, seed = global_seed){
  set.seed(seed)
  z1 <- rnorm(n, 0, 1)
  z2 <- rnorm(n, 0, 1)
  z3 <- rnorm(n, 0, 1)
  z4 <- rnorm(n, 0, 1)
  
  y <- 210 + 27.4*z1 + 13.7*z2 + 13.7 * z3 + 13.7 * z4 + rnorm(n, 0, 1)
  pi <- expit(- z1 + 0.5 * z2 - 0.25 * z3 - 0.1 * z4 + rnorm(n, 0, 1))
  x1 <- exp(z1/2)
  x2 <- z2/(1 + exp(z1)) + 10
  x3 <- (z1 * z3/25 +.6)^3
  x4 <- (z2 + z4 + 20)^2
  t <- round(pi)
  ret <- tibble(y, x1, x2, x3, x4, z1, z2, z3, z4, p_id = 1:n, pi, t = round(pi) ) 
  prop_model_x <- glm(t ~ x1 + x2 + x3 + x4, family = binomial(), data = ret) %>% predict(type = "response")
  prop_model_z <- glm(t ~ z1 + z2 + z3 + z4, family = binomial(), data = ret) %>% predict(type = "response")
  y_model_x <- lm(y ~ x1 + x2 + x3 + x4, data = ret) %>% predict()
  y_model_z <- lm(y ~ z1 + z2 + z3 + z4, data = ret) %>% predict()
  
  ret %>% mutate(prop_correct = prop_model_z, 
                 prop_wrong = prop_model_x, 
                 y_pred_correct = y_model_z, 
                 y_pred_wrong = y_model_x)
}

