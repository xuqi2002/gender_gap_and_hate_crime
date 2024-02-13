#### Preamble ####
# Purpose: Help Functions for Figure 2 Data Processing 
# Author: Yixuan Yao
# Date: 12 February 2024
# Contact: 
# License: MIT
# Pre-requisites: none

marginal_effect <- function(out,
                            newdata=NULL, 
                            main_var, family = "logit",
                            treat_range, difference = FALSE,
                            seed=1234){
  
  fit  <- out$fit
  # Coef and VCOV 
  coef_mar <- coef(out$fit)[is.na(coef(out$fit)) == FALSE]
  vcov_mar <- out$vcov
  
  # Sample Mean of Outcomes
  y_orig <- model.frame(formula(fit), data = newdata)[ ,1]
  sample_mean_outcome <- mean(y_orig, na.rm = TRUE)
  
  # Prepare model.frame and treat_range
  newdata_use_b <- model.frame(formula(fit), data = newdata)
  if(missing(treat_range)){
    treat_range   <- quantile(newdata_use_b[, main_var], 
                              c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                              na.rm = TRUE)
  }
  
  # Create new_treat and new_control automatically
  newdata_use_l  <- list()
  for(i in 1:length(treat_range)){
    newdata_use_l_b <- newdata_use_b
    newdata_use_l_b[ , main_var] <- treat_range[i]
    newdata_use_l_exp <- model.matrix(formula(fit), data = newdata_use_l_b)
    newdata_use_l_exp <- newdata_use_l_exp[, names(coef_mar)]
    newdata_use_l[[i]] <- newdata_use_l_exp
  }
  
  set.seed(seed)
  sim_coef <- mvrnorm(n = 1000, mu=coef_mar, Sigma=vcov_mar)
  
  # Linear Part 
  linear_out <- lapply(newdata_use_l, 
                       FUN = function(x) as.matrix(sim_coef) %*% as.matrix(t(x)))
  
  
  if(family %in% c("nb", "poisson")){
    out <- lapply(linear_out, FUN = function(x) apply(exp(x), 1, mean))
  }else if(family %in% c("ols")){
    out <- lapply(linear_out, FUN = function(x) apply(x, 1, mean))
  }else if(family %in% c("logit")){
    out <- lapply(linear_out, FUN = function(x) apply((exp(x)/(1 + exp(x))), 1, mean))
  }else{
    warning("family should be one of 'nb', 'poisson', 'logit', and 'ols'")
  }
  
  if(difference == FALSE){
    out_main <- lapply(out, FUN = function(x) c(quantile(x, c(0.025)), mean(x), quantile(x, c(0.975))))
    names(out_main) <- treat_range
    out_main_percent <- lapply(out, 
                               FUN = function(x) 
                                 c(quantile(x, c(0.025)), mean(x), quantile(x, c(0.975)))/sample_mean_outcome)
    names(out_main_percent) <- treat_range
  }else if(difference == TRUE){
    out_b <- out[[2]]  - out[[1]]
    out_main <- c(quantile(out_b, c(0.025)), mean(out_b), quantile(out_b, c(0.975)), 
                  quantile(out_b, c(0.05)), quantile(out_b, c(0.95)))
    out_main_percent <- out_main/sample_mean_outcome
  }
  
  output <- list("out" = out, 
                 "out_main" = out_main,
                 "out_main_percent" = out_main_percent,
                 "sample_mean" = sample_mean_outcome,
                 "treat_range" = treat_range)
}

bin.summary <- function(formula, print_var, id, data, digits = 3, type = "logit"){
  var  <- all.vars(formula)
  data_use <- model.frame( ~ ., data = data[, c(var, id)])
  
  if(type == "logit")  fit <- glm(formula, data = data_use, family = "binomial")
  if(type == "probit") fit <- glm(formula, data = data_use, family = binomial(link="probit"))
  tab_p <- coeftest(fit, vcov = vcovCL(fit, cluster = data_use[, id]))
  
  if(missing(print_var)) print_var <- seq(1:min(20, nrow(tab_p)))
  
  mat <- tab_p[print_var, ]
  
  sig <- rep("", length(mat[,4]))
  sig[mat[ , 4]  < 0.001] <- "***"
  sig[mat[ , 4]  >= 0.001 & mat[ , 4]  < 0.01] <- "**"
  sig[mat[ , 4]  >= 0.01 & mat[ , 4]  < 0.05] <- "*"
  sig[mat[ , 4]  >= 0.05 & mat[ , 4]  < 0.1] <- "."
  mat <- as.data.frame(mat)
  mat <- round(mat, digits = digits)
  mat$Sig <- sig 
  
  sample_size <- length(fit$residuals)
  
  cat("Coefficients:\n")
  print(mat[, c(1, 2, 4, 5)], row.names=TRUE)
  cat(paste("(Sample Size:", sample_size, ")\n", sep = ""))
  
  output <- list("fit" = fit, "vcov" = vcovCL(fit, cluster = data_use[, id]),
                 "sample" = sample_size)
  
  return(output)
}

lm.summary <- function(formula, print_var, id, data, digits = 3){
  var  <- all.vars(formula)
  data_use <- model.frame( ~ ., data = data[, c(var, id)])
  
  fit <- lm(formula, data = data_use)
  tab_p <- coeftest(fit, vcov = vcovCL(fit, cluster = data_use[, id]))
  
  if(missing(print_var)) print_var <- seq(1:min(20, nrow(tab_p)))
  
  mat <- tab_p[print_var, ]
  
  sig <- rep("", length(mat[,4]))
  sig[mat[ , 4]  < 0.001] <- "***"
  sig[mat[ , 4]  >= 0.001 & mat[ , 4]  < 0.01] <- "**"
  sig[mat[ , 4]  >= 0.01 & mat[ , 4]  < 0.05] <- "*"
  sig[mat[ , 4]  >= 0.05 & mat[ , 4]  < 0.1] <- "."
  mat <- as.data.frame(mat)
  mat <- round(mat, digits = digits)
  mat$Sig <- sig 
  
  sample_size <- length(fit$residuals)
  
  cat("Coefficients:\n")
  print(mat[, c(1, 2, 4, 5)], row.names=TRUE)
  cat(paste("(Sample Size:", sample_size, ")\n", sep = ""))
  
  output <- list("fit" = fit, "vcov" = vcovCL(fit, cluster = data_use[, id]),
                 "sample" = sample_size)
  
  return(output)
}


glm.boot <- function(formula, data, family, cluster_id, boot = 1000, seed = 1234){
  
  set.seed(seed)
  data$cluster_id <- cluster_id
  
  data_u <- data[is.na(data$cluster_id) ==  FALSE, ]
  coef_boot <- c()
  for(b in 1:boot){
    boot_id <- sample(unique(data_u$cluster_id), size = length(unique(data_u$cluster_id)), replace=TRUE)
    # create bootstap sample with sapply
    boot_which <- sapply(boot_id, function(x) which(data_u$cluster_id == x))
    data_boot <- data_u[unlist(boot_which),]
    if(family == "poisson"){
      glm_boot <- glm(formula, family = "poisson", data = data_boot)
      coef_boot <-  cbind(coef_boot, summary(glm_boot)$coef[,1])
    }else if(family == "negative-binomial"){
      glm_nb_boot <- glm.nb(formula, data = data_boot)
      coef_boot <-  cbind(coef_boot, summary(glm_nb_boot)$coef[,1])
    }
    if((b%%100) == 0) cat(paste(b, "..."))
  }
  if(family == "poisson"){
    glm_boot_final <- glm(formula, family = "poisson", data = data)
  }else if(family == "negative-binomial"){
    glm_boot_final <- glm.nb(formula, data = data)
  }
  se <- apply(coef_boot, 1, sd) # bootstrap SE
  coef <- glm_boot_final$coefficients
  
  output <- list("fit" = glm_boot_final, "coef" = coef,  "se" = se)
  
  return(output)
}

star_out <- function(out, name){
  writeLines(capture.output(out), name)
}
