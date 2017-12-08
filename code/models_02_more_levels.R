library(tidyverse)
library(R2jags)

options(scipen=999)

building_classification <- read_delim("../info/Building_Classification.psv", delim = "|") %>% 
  rename(Building_Class_Description = Description)

nyc_sales <- read_rds("../out/nyc_sales.rds") %>% 
  filter(!is.na(Borough),
         log(SALE_PRICE) > quantile(log(SALE_PRICE), 0.025),
         log(SALE_PRICE) < quantile(log(SALE_PRICE), 0.975)) %>% 
  mutate(year_cat = cut(YEAR_BUILT, breaks = c(-Inf, 1900, 1930, 1945, 1970, Inf))) %>% 
  left_join(building_classification, by = c("BUILDING_CLASS_AT_TIME_OF_SALE" = "Code"))

nyc_train <- nyc_sales %>% 
  group_by(zip_code) %>% 
  sample_frac(0.9)

nyc_test <- nyc_sales %>% 
  filter(!(id %in% nyc_train$id))

nyc_train_list <- list(
  y = log(nyc_train$SALE_PRICE),
  x = log(nyc_train$GROSS_SQUARE_FEET),
  building_class = as.integer(as.factor(nyc_train$BUILDING_CLASS_CATEGORY)),
  borough = as.integer(as.factor(nyc_train$Borough)),
  neighborhood = as.integer(as.factor(nyc_train$Neighborhood)),
  n_borough = length(unique(nyc_train$Borough)),
  n_neighborhood = length(unique(nyc_train$Neighborhood)),
  n = nrow(nyc_train),
  n_zip = length(unique(nyc_train$zip_code)),
  zip_code = as.integer(as.factor(nyc_train$zip_code)),
  y_test = log(nyc_test$SALE_PRICE),
  x_test = log(nyc_test$GROSS_SQUARE_FEET),
  building_class_test = as.integer(as.factor(nyc_test$BUILDING_CLASS_CATEGORY)),
  borough_test = as.integer(as.factor(nyc_test$Borough)),
  neighborhood_test = as.integer(as.factor(nyc_test$Neighborhood)),
  n_borough_test = length(unique(nyc_test$Borough)),
  n_neighborhood_test = length(unique(nyc_test$Neighborhood)),
  n_test = nrow(nyc_test),
  n_zip_test = length(unique(nyc_test$zip_code)),
  zip_code_test = as.integer(as.factor(nyc_test$zip_code))
)



## Modelo 6: Jerárquico con zip codes, tipo de edificio como covariable y pendientes e interceptos distintos

string_mod_6 <- "model {
for(i in 1:n) {
y[i] ~ dnorm(mu[i], tau.y) 
mu[i] <- alpha[[i]] + beta_1[neighborhood[i]]*x[i] + beta_2[neighborhood[i]]*building_class[i] + beta_3[neighborhood[i]]*building_class[i]*x[i]
}
tau.y ~ dgamma(0.001, 0.001)
for(j in 1:n_neighborhood){
alpha[j] ~ dnorm(mu.a, tau.a)
beta_1[j] ~ dnorm(mu.b1, tau.b1)
beta_2[j] ~ dnorm(mu.b2, tau.b2)
beta_3[j] ~ dnorm(mu.b3, tau.b3)
}
mu.a ~ dnorm(0, 0.0001)
tau.a ~ dgamma(0.001, 0.001)
mu.b1 ~ dnorm(0, 0.0001)
tau.b1 ~ dgamma(0.001, 0.001)
mu.b2 ~ dnorm(0, 0.0001)
tau.b2 ~ dgamma(0.001, 0.001)
mu.b3 ~ dnorm(0, 0.0001)
tau.b3 ~ dgamma(0.001, 0.001)

# Train predictions
#for(i in 1:n){
#yf[i] ~ dnorm(mu[i], tau.y) 
#}

# Test predictions
#for(i in 1:n_test){
#yf_test[i] ~ dnorm(mu_test[i], tau.y) 
#mu_test[i] <- alpha[zip_code_test[i]] + 
#beta_1[zip_code_test[i]]*x_test[i] + 
#beta_2[zip_code_test[i]]*building_class_test[i] + 
#beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
#}
    
}"

write_file(string_mod_6,
           path = "model_6.model")

inits_6 <- function(){
  list(
    alpha = rep(0, nyc_train_list$n_zip), 
    beta_1 = rep(0, nyc_train_list$n_zip), 
    beta_2 = rep(0, nyc_train_list$n_zip), 
    beta_3 = rep(0, nyc_train_list$n_zip), 
    tau.y = 1,
    mu.a = 0,
    tau.a = 1,
    mu.b1 = 0,
    tau.b1 = 1,
    mu.b2 = 0,
    tau.b2 = 1,
    mu.b3 = 0,
    tau.b3 = 1
  )
}

parameters_6 <- c("mu.a", 
                  "tau.a", 
                  "alpha", 
                  "beta_1", 
                  "beta_2", 
                  "beta_3", 
                  "mu.b1",
                  "tau.b1",
                  "mu.b2",
                  "tau.b2",
                  "mu.b3",
                  "tau.b3",
                  "tau.y", 
                  "yf",
                  "yf_test")

sim_6 <- jags(nyc_train_list,
              inits_6,
              parameters_6,
              model.file = "model_6.model",
              n.iter = 10000,
              n.chains = 1,
              n.thin = 1,
              n.burnin = 1000)

# saveRDS(sim_6, "../out/models/model_06.rds")


sim_6 <- read_rds("../out/models/model_06.rds")

#traceplot(sim_6)

# Con 1000 iteraciones tardó 91 segundos

sim_6$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))


preds_6 <- sim_6$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_6 <- sim_6$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_6 <- sim_6$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

rmse_train_6 <- mean(preds_6$res^2)
rmse_train_log_6 <- mean((preds_6$mean - log(preds_6$obs))^2)

(preds_6 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_6)

preds_6 %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res), size = 0.7, alpha = 0.6)

preds_6 %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res), size = 0.7, alpha = 0.6)

preds_6 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res, color = BUILDING_CLASS_CATEGORY), size = 0.8, alpha = 0.6)


preds_6 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_6 %>% 
  sample_n(6000) %>% 
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.4, size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
  geom_abline(slope = 1)

preds_6 %>% 
  sample_n(5000) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(alpha = 0.4) +
  geom_errorbar(aes(ymin = exp(X2.5.), ymax = exp(X97.5.)), alpha = 0.4) +
  geom_abline(slope = 1)

preds_6 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_6 %>% 
  mutate(Borough = nyc_train$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
  geom_abline(slope = 1)





