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



### Modelo de unidades iguales (complete pooling)

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

if("summary_mod_comp_pooling.rds" %in% file_list){
  summary_mod_comp_pooling <- read_rds("../out/models/summary_mod_comp_pooling.rds")
} else {
  summary_mod_comp_pooling <- read_rds("../out/models/model_comp_pooling.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_comp_pooling, "../out/models/summary_mod_comp_pooling.rds")
  gc()
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

## Convergence diagnostocs

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  ggplot() + 
  geom_point(aes(rowname, Rhat)) +
  geom_hline(yintercept = 1.2, color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("yf", rowname)) %>% 
  filter(grepl("test", rowname)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() + 
  geom_point(aes(ix, Rhat), size = 0.3, alpha = 0.5) +
  geom_hline(yintercept = 1.2, color = 'grey', linetype = 'dashed') +
  expand_limits(y = 0)

summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  mutate(prop_n_eff = n.eff/4000) %>% 
  ggplot() + 
  geom_point(aes(rowname, prop_n_eff)) +
  geom_hline(yintercept = 1, color = 'grey', linetype = 'dashed') +
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



