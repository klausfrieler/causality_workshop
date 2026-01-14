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
foo <- simulate_class_trick(seed = 786543)

d <- function(a) {
  epsilon <- runif(length(a))
  ifelse(epsilon < 0.5 & a == 1, 0, a)
}

get_density_ratios <- function(data = foo, seed = 786543) {
  set.seed(seed)
  # Define the intervention as a function
  # Create an empty matrix to store the density ratios
  density_ratios <- matrix(NA, nrow = nrow(data), ncol = 2)
  
  for (t in 1:2) {
    a <- c("A1", "A2")[t]
    # Duplicate the data
    shifted <- data
    # In the duplicated data, modify treatment according to d
    shifted[[a]] <- d(foo[[a]])
    
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

sdr_man <- function(data = foo){
  
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
simulate_20_1 <- function(n, mult = 1, sd = 10){
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
  map_dfr(1:nrow(spec), function(i){
    tibble( 
           Y = rnorm(spec$N[i] * mult, mean = spec$Y[i], sd = sd),    
           A0 = spec$A0[i], 
           L1 = spec$L1[i], 
           A1 = spec$A1[i])
  }) %>% mutate(p_id = 1:nrow(.))   
}
