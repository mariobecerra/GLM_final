library(tidyverse)
library(R2jags)

options(scipen=999)
theme_set(theme_bw())

building_classification <- read_delim("../info/Building_Classification.psv", delim = "|") %>% 
  rename(Building_Class_Description = Description)

nyc_sales <- read_rds("../out/nyc_sales.rds") %>% 
  filter(!is.na(Borough),
         log(SALE_PRICE) > quantile(log(SALE_PRICE), 0.025),
         log(SALE_PRICE) < quantile(log(SALE_PRICE), 0.975)) %>% 
  mutate(year_cat = cut(YEAR_BUILT, breaks = c(-Inf, 1900, 1930, 1945, 1970, Inf))) %>% 
  left_join(building_classification, by = c("BUILDING_CLASS_AT_TIME_OF_SALE" = "Code")) %>% 
  mutate(
    building_class_int = as.integer(as.factor(BUILDING_CLASS_CATEGORY)),
    borough_int = as.integer(as.factor(Borough)),
    neighborhood_int = as.integer(as.factor(Neighborhood)),
    zip_code_int = as.integer(as.factor(zip_code))  
  )

file_list <- list.files("../out/models")

set.seed(124362)

nyc_train <- nyc_sales %>% 
  group_by(zip_code) %>% 
  sample_frac(0.9)

nyc_test <- nyc_sales %>% 
  filter(!(id %in% nyc_train$id))


# Datos para JAGS

nyc_sales_list <- list(
  y = log(nyc_train$SALE_PRICE),
  x = log(nyc_train$GROSS_SQUARE_FEET),
  building_class = nyc_train$building_class_int,
  borough = nyc_train$borough_int,
  neighborhood = nyc_train$neighborhood_int,
  zip_code = nyc_train$zip_code_int,
  n_borough = length(unique(nyc_train$Borough)),
  n_neighborhood = length(unique(nyc_train$Neighborhood)),
  n = nrow(nyc_train),
  n_zip = length(unique(nyc_train$zip_code)),
  y_test = log(nyc_test$SALE_PRICE),
  x_test = log(nyc_test$GROSS_SQUARE_FEET),
  building_class_test = nyc_test$building_class_int,
  borough_test = nyc_test$borough_int,
  neighborhood_test = nyc_test$neighborhood_int,
  zip_code_test = nyc_test$zip_code_int,
  n_borough_test = length(unique(nyc_test$Borough)),
  n_neighborhood_test = length(unique(nyc_test$Neighborhood)),
  n_test = nrow(nyc_test),
  n_zip_test = length(unique(nyc_test$zip_code))
)


######################################################
######################################################
### Modelo de unidades iguales (complete pooling)
######################################################
######################################################

string_mod_comp_pooling <- "model {
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*x[i]
  }
  #Priors 
  alpha ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
  
  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau) 
  }
  
  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau) 
    mu_test[i] <- alpha + beta*x_test[i]
  }
}"

write_file(string_mod_comp_pooling,
           path = "model_comp_pooling.model")

inits_comp_pooling <- function(){
  list(
    alpha = rnorm(1), 
    beta = rnorm(1),
    tau = (runif(1, 0, 100)^-2)
  )
}

parameters_comp_pooling <- c("alpha", "beta", "tau", "yf", "yf_test")

sim_comp_pooling <- jags(nyc_sales_list,
              inits_comp_pooling,
              parameters_comp_pooling,
              model.file = "model_comp_pooling.model",
              n.iter = 3000,
              n.chains = 4,
              n.thin = 2,
              n.burnin = 1000)

saveRDS(sim_comp_pooling, "../out/models/model_comp_pooling.rds")
summary_mod_comp_pooling <- sim_comp_pooling$BUGSoutput$summary
saveRDS(summary_mod_comp_pooling, "../out/models/summary_mod_comp_pooling.rds")

if(!("summary_mod_comp_pooling" %in% objects())){
  if("summary_mod_comp_pooling.rds" %in% file_list){
    summary_mod_comp_pooling <- read_rds("../out/models/summary_mod_comp_pooling.rds")
  } else {
    summary_mod_comp_pooling <- read_rds("../out/models/model_comp_pooling.rds")$BUGSoutput$summary  
    saveRDS(summary_mod_comp_pooling, "../out/models/summary_mod_comp_pooling.rds")
    gc()
  }
}

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_comp_pooling <- summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_comp_pooling <- summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

# Percentage of observations inside the 95% probability interval
(preds_comp_pooling %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_comp_pooling)

(preds_test_comp_pooling %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_comp_pooling)


(rmse_train_comp_pooling <- sqrt(mean(preds_comp_pooling$res^2)))
(rmse_train_log_comp_pooling <- sqrt(mean((preds_comp_pooling$mean - log(preds_comp_pooling$obs))^2)))
(rmse_test_comp_pooling <- sqrt(mean(preds_test_comp_pooling$res^2)))
(rmse_test_log_comp_pooling <- sqrt(mean((preds_test_comp_pooling$mean - log(preds_test_comp_pooling$obs))^2)))

###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  ggplot() + 
  geom_point(aes(rowname, Rhat)) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

# Effective sample size

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  ggplot() + 
  geom_point(aes(rowname, n.eff)) +
  geom_hline(yintercept = 4000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 4000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 4000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)






######################################################
######################################################
### Modelo de unidades distintas (no pooling)
######################################################
######################################################

string_mod_no_pooling <- "model {
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha[zip_code[i]] + beta[zip_code[i]]*x[i]
  }
  #Priors 
  for(j in 1:n_zip){
    alpha[j] ~ dnorm(0, 0.001)
    beta[j] ~ dnorm(0, 0.001)
  }
  tau ~ dgamma(0.001, 0.001)
  
  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau) 
  }
  
  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau) 
    mu_test[i] <- alpha[zip_code_test[i]] + beta[zip_code_test[i]]*x_test[i]
  }
}"

write_file(string_mod_no_pooling,
           path = "model_no_pooling.model")

inits_no_pooling <- function(){
  list(
    alpha = rnorm(nyc_sales_list$n_zip), 
    beta = rnorm(nyc_sales_list$n_zip),
    tau = (runif(1, 0, 100)^-2)
  )
}

parameters_no_pooling <- c("alpha", "beta", "tau", "yf", "yf_test")

sim_no_pooling <- jags(nyc_sales_list,
                       inits_no_pooling,
                       parameters_no_pooling,
                       model.file = "model_no_pooling.model",
                       n.iter = 4000,
                       n.thin = 2,
                       n.chains = 4,
                       n.burnin = 2000)

saveRDS(sim_no_pooling, "../out/models/model_no_pooling.rds")
summary_mod_no_pooling <- sim_no_pooling$BUGSoutput$summary
saveRDS(summary_mod_no_pooling, "../out/models/summary_mod_no_pooling.rds")

if(!("summary_mod_no_pooling" %in% objects())){
  if("summary_mod_no_pooling.rds" %in% file_list){
    summary_mod_no_pooling <- read_rds("../out/models/summary_mod_no_pooling.rds")
  } else {
    summary_mod_no_pooling <- read_rds("../out/models/model_no_pooling.rds")$BUGSoutput$summary  
    saveRDS(summary_mod_no_pooling, "../out/models/summary_mod_no_pooling.rds")
    gc()
  }
}

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_no_pooling <- summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_no_pooling <- summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

# Percentage of observations inside the 95% probability interval
(preds_no_pooling %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_no_pooling)

(preds_test_no_pooling %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_no_pooling)


(rmse_train_no_pooling <- sqrt(mean(preds_no_pooling$res^2)))
(rmse_train_log_no_pooling <- sqrt(mean((preds_no_pooling$mean - log(preds_no_pooling$obs))^2)))
(rmse_test_no_pooling <- sqrt(mean(preds_test_no_pooling$res^2)))
(rmse_test_log_no_pooling <- sqrt(mean((preds_test_no_pooling$mean - log(preds_test_no_pooling$obs))^2)))

###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.7) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

# Effective sample size

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.7) +
  geom_hline(yintercept = 4000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 4000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 4000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)


summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  set_names(make.names(names(.))) %>% 
  filter(grepl("alpha", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot(aes(x = ix, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.))

summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  set_names(make.names(names(.))) %>% 
  filter(grepl("beta", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot(aes(x = ix, y = mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.))




######################################################
######################################################
### Partial pooling, three levels: borough, neighborhood & zip code
######################################################
######################################################

string_mod_three_levels <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
    mu[i] <-  alpha[zip_code[i]] + 
              beta_1[zip_code[i]]*x[i] 
  }
  
  for(j in 1:n_zip){
    alpha[j] ~ dnorm(mu.a[neighborhood[j]], tau.a[neighborhood[j]])
    beta_1[j] ~ dnorm(mu.b1[neighborhood[j]], tau.b1[neighborhood[j]])
    tau.y[j] ~ dgamma(alpha.y[borough[j]], beta.y[borough[j]])
  }
  
  for(j in 1:n_neighborhood){
    mu.a[j] ~ dnorm(mu.a.1[borough[j]], tau.a.1[borough[j]])
    tau.a[j] ~ dexp(lambda.a.1[borough[j]])
    mu.b1[j] ~ dnorm(mu.b1.1[borough[j]], tau.b1.1[borough[j]])
    tau.b1[j] ~ dexp(lambda.b1.1[borough[j]])
    alpha.y[j] ~ dexp(lambda.alpha.y[borough[j]])
    beta.y[j] ~ dexp(lambda.beta.y[borough[j]])
  }
  
  for(j in 1:n_borough){
    mu.a.1[j] ~ dnorm(mu.a.1.0, tau.a.1.0)
    tau.a.1[j] ~ dexp(lambda.a.tau.0)
    lambda.a.1[j] ~ dexp(lambda.a.0)
    
    mu.b1.1[j] ~ dnorm(mu.b1.1.0, tau.b1.1.0)
    tau.b1.1[j] ~ dexp(lambda.b1.tau.0)
    lambda.b1.1[j] ~ dexp(lambda.b1.0)
    
    lambda.alpha.y[j] ~ dexp(lambda.lambda.alpha.y)
    lambda.beta.y[j] ~ dexp(lambda.lambda.beta.y)
  }
  
  mu.a.1.0 ~ dnorm(0, 0.0001)
  tau.a.1.0 ~ dgamma(0.01, 0.01)
  lambda.a.tau.0 ~ dgamma(0.01, 0.01)
  lambda.a.0 ~ dgamma(0.01, 0.01)
  mu.b1.1.0 ~ dnorm(0, 0.0001)
  tau.b1.1.0 ~ dgamma(0.01, 0.01)
  lambda.b1.tau.0 ~ dgamma(0.01, 0.01)
  lambda.b1.0 ~ dgamma(0.01, 0.01)
  lambda.lambda.alpha.y ~ dgamma(0.01, 0.01)
  lambda.lambda.beta.y ~ dgamma(0.01, 0.01)
  
  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
  }
  
  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y[zip_code_test[i]]) 
    mu_test[i] <- alpha[zip_code_test[i]] + 
                  beta_1[zip_code_test[i]]*x_test[i] 
  }

}"

write_file(string_mod_three_levels,
           path = "model_three_levels.model")

inits_three_levels <- function(){
  list(
    tau.y = runif(nyc_sales_list$n_zip, 0.1, 100),
    alpha = rnorm(nyc_sales_list$n_zip), 
    beta_1 = rnorm(nyc_sales_list$n_zip), 
    mu.a = rnorm(nyc_sales_list$n_neighborhood),
    mu.b1 = rnorm(nyc_sales_list$n_neighborhood),
    tau.a = runif(nyc_sales_list$n_neighborhood, 0.1, 100),
    tau.b1 = runif(nyc_sales_list$n_neighborhood, 0.1, 100),
    alpha.y = runif(nyc_sales_list$n_neighborhood, 0.1, 100),
    beta.y = runif(nyc_sales_list$n_neighborhood, 0.1, 100),
    
    mu.a.1 = rnorm(nyc_sales_list$n_borough),
    tau.a.1 = runif(nyc_sales_list$n_borough, 0.1, 100),
    lambda.a.1 = runif(nyc_sales_list$n_borough, 0.1, 100),
    mu.b1.1 = rnorm(nyc_sales_list$n_borough),
    tau.b1.1 = runif(nyc_sales_list$n_borough, 0.1, 100),
    lambda.b1.1 = runif(nyc_sales_list$n_borough, 0.1, 100),
    lambda.alpha.y = runif(nyc_sales_list$n_borough, 0.1, 100),
    lambda.beta.y = runif(nyc_sales_list$n_borough, 0.1, 100),
    
    mu.a.1.0 = rnorm(1),
    tau.a.1.0 = runif(1, 0.5, 3.5),
    lambda.a.tau.0 = runif(1, 0.5, 3.5),
    lambda.a.0 = runif(1, 0.5, 3.5),
    mu.b1.1.0 = rnorm(1),
    tau.b1.1.0 = runif(1, 0.5, 3.5),
    lambda.b1.tau.0 = runif(1, 0.5, 3.5),
    lambda.b1.0 = runif(1, 0.5, 3.5),
    lambda.lambda.alpha.y = runif(1, 0.5, 3.5),
    lambda.lambda.beta.y = runif(1, 0.5, 3.5)
  )
}

parameters_three_levels <- c("tau.y",
                   "alpha",
                   "beta_1",
                   "mu.a",
                   "mu.b1",
                   "tau.a",
                   "tau.b1",
                   "alpha.y",
                   "beta.y",
                   
                   "mu.a.1",
                   "tau.a.1",
                   "lambda.a.1",
                   "mu.b1.1",
                   "tau.b1.1",
                   "lambda.b1.1",
                   "lambda.alpha.y",
                   "lambda.beta.y",
                   
                   "mu.a.1.0",
                   "tau.a.1.0",
                   "lambda.a.tau.0",
                   "lambda.a.0",
                   "mu.b1.1.0",
                   "tau.b1.1.0",
                   "lambda.b1.tau.0",
                   "lambda.b1.0",
                   "lambda.lambda.alpha.y",
                   "lambda.lambda.beta.y",
                   
                   "yf",
                   "yf_test"
                   
)

sim_three_levels <- jags(nyc_sales_list,
               inits_three_levels,
               parameters_three_levels,
               model.file = "model_three_levels.model",
               n.iter = 15000,
               n.chains = 4,
               n.thin = 5,
               n.burnin = 5000)

saveRDS(sim_three_levels, "../out/models/model_three_levels.rds")
summary_mod_three_levels <- sim_three_levels$BUGSoutput$summary
saveRDS(summary_mod_three_levels, "../out/models/summary_mod_three_levels.rds")


if(!("summary_mod_three_levels" %in% objects())){
  if("summary_mod_three_levels.rds" %in% file_list){
    summary_mod_three_levels <- read_rds("../out/models/summary_mod_three_levels.rds")
  } else {
    summary_mod_three_levels <- read_rds("../out/models/model_three_levels.rds")$BUGSoutput$summary  
    saveRDS(summary_mod_three_levels, "../out/models/summary_mod_three_levels.rds")
    gc()
  }
}

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_three_levels <- summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_three_levels <- summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

# Percentage of observations inside the 95% probability interval
(preds_three_levels %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_three_levels)

(preds_test_three_levels %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_three_levels)


(rmse_train_three_levels <- sqrt(mean(preds_three_levels$res^2)))
(rmse_train_log_three_levels <- sqrt(mean((preds_three_levels$mean - log(preds_three_levels$obs))^2)))
(rmse_test_three_levels <- sqrt(mean(preds_test_three_levels$res^2)))
(rmse_test_log_three_levels <- sqrt(mean((preds_test_three_levels$mean - log(preds_test_three_levels$obs))^2)))

###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.7) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, 
             linetype = 'dashed', 
             size = 1, 
             color = 'black', 
             alpha = 0.6) +
  expand_limits(y = 1)

# Effective sample size

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.7) +
  geom_hline(yintercept = 8000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 8000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
  geom_hline(yintercept = 8000, 
             color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)


