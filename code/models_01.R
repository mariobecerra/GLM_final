library(tidyverse)
library(R2jags)

building_classification <- read_delim("../info/Building_Classification.psv", delim = "|") %>% 
  rename(Building_Class_Description = Description)

nyc_sales <- read_rds("../out/nyc_sales.rds") %>% 
  filter(!is.na(Borough),
         log(SALE_PRICE) > quantile(log(SALE_PRICE), 0.025),
         log(SALE_PRICE) < quantile(log(SALE_PRICE), 0.975)) %>% 
  mutate(year_cat = cut(YEAR_BUILT, breaks = c(-Inf, 1900, 1930, 1945, 1970, Inf))) %>% 
  left_join(building_classification, by = c("BUILDING_CLASS_AT_TIME_OF_SALE" = "Code"))

nyc_sales_list <- list(
  y = log(nyc_sales$SALE_PRICE),
  x = log(nyc_sales$GROSS_SQUARE_FEET),
  building_class = as.integer(as.factor(nyc_sales$BUILDING_CLASS_CATEGORY)),
  borough = as.integer(as.factor(nyc_sales$Borough)),
  neighborhood = as.integer(as.factor(nyc_sales$Neighborhood)),
  n_borough = length(unique(nyc_sales$Borough)),
  n_neighborhood = length(unique(nyc_sales$Neighborhood)),
  n = nrow(nyc_sales)
)

## Modelo 1: unidades iguales

string_mod_1 <- "model {
  #Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- alpha + beta*x[i]
  }
  #Priors 
  alpha ~ dnorm(0, 0.001)
  beta ~ dnorm(0, 0.001)
  tau ~ dgamma(0.001, 0.001)
}"

write_file(string_mod_1,
           path = "model_1.model")

inits_1 <- function(){
  list(
    alpha = 0, 
    beta = 0,
    tau = 1
  )
}

parameters_1 <- c("alpha", "beta", "tau")

sim_1 <- jags(nyc_sales_list,
              inits_1,
              parameters_1,
              model.file = "model_1.model",
              n.iter = 10000,
              n.chains = 1,
              n.burnin = 1000)

# Con 10000 iteraciones tardó minuto y medio



## Modelo 2: Jerárquico con boroughs

string_mod_2 <- "model {
  for(i in 1 : n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <- alpha[borough[i]] + beta*x[i]
  }
  beta ~ dnorm(0, 0.0001)
  tau.y ~ dgamma(0.001, 0.001)
  for(j in 1:n_borough){
    alpha[j] ~ dnorm(mu.a, tau.a)
  }
  mu.a ~ dnorm(0, 0.0001)
  tau.a ~ dgamma(0.001, 0.001)
}"

write_file(string_mod_2,
           path = "model_2.model")

inits_2 <- function(){
  list(
    alpha = rep(0, 5), 
    beta = 0,
    tau.y = 1,
    mu.a = 0,
    tau.a = 1
  )
}

parameters_2 <- c("mu.a", "tau.a", "alpha", "beta", "tau.y")

sim_2 <- jags(nyc_sales_list,
              inits_2,
              parameters_2,
              model.file = "model_2.model",
              n.iter = 1000,
              n.chains = 1,
              n.thin = 1,
              n.burnin = 300)

traceplot(sim_2)

# Para 20000 iteraciones tardó 3 minutos


## Modelo 3: Jerárquico con zip codes

string_mod_3 <- "model {
  for(i in 1 : n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <- alpha[neighborhood[i]] + beta*x[i]
  }
  beta ~ dnorm(0, 0.0001)
  tau.y ~ dgamma(0.001, 0.001)
  for(j in 1:n_neighborhood){
    alpha[j] ~ dnorm(mu.a, tau.a)
  }
  mu.a ~ dnorm(0, 0.0001)
  tau.a ~ dgamma(0.001, 0.001)

  # Predictions

  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }
}"

write_file(string_mod_3,
           path = "model_3.model")

inits_3 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_neighborhood), 
    beta = 0,
    tau.y = 1,
    mu.a = 0,
    tau.a = 1
  )
}

parameters_3 <- c("mu.a", "tau.a", "alpha", "beta", "tau.y", "yf")

sim_3 <- jags(nyc_sales_list,
              inits_3,
              parameters_3,
              model.file = "model_3.model",
              n.iter = 1000,
              n.chains = 1,
              n.thin = 1,
              n.burnin = 300)

traceplot(sim_3)

# Con 1000 iteraciones tardó 15 segundos


preds_3 <- sim_3$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_sales$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_3 %>% 
  mutate(ix = 1:nrow(.)) %>% 
  sample_n(1000) %>% 
  ggplot() +
  geom_point(aes(ix, res))


preds_3 %>% 
  sample_n(5000) %>% 
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.6) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.6) +
  geom_abline(slope = 1)

preds_3 %>% 
  sample_n(5000) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(alpha = 0.6) +
  geom_errorbar(aes(ymin = exp(X2.5.), ymax = exp(X97.5.)), alpha = 0.6) +
  geom_abline(slope = 1)


preds_3 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY,
         Borough = nyc_sales$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_3 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY,
         Borough = nyc_sales$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
  geom_abline(slope = 1)

preds_3 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY,
         Borough = nyc_sales$Borough,
         year = nyc_sales$year_cat) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = year), alpha = 0.6) +
  geom_abline(slope = 1)

preds_3 %>% 
  mutate(Address = nyc_sales$ADDRESS) %>% 
  filter(log(obs) < 12, mean > 12) %>% 
  View



## Modelo 4: Jerárquico con zip codes y tipo de edificio como covariable

string_mod_4 <- "model {
  for(i in 1 : n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <- alpha[neighborhood[i]] + beta_1*x[i] + beta_2*building_class[i] + beta_3*building_class[i]*x[i]
  }
  beta_1 ~ dnorm(0, 0.0001)
  beta_2 ~ dnorm(0, 0.0001)
  beta_3 ~ dnorm(0, 0.0001)
  tau.y ~ dgamma(0.001, 0.001)
  for(j in 1:n_neighborhood){
    alpha[j] ~ dnorm(mu.a, tau.a)
  }
  mu.a ~ dnorm(0, 0.0001)
  tau.a ~ dgamma(0.001, 0.001)

  # Predictions

  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }
}"

write_file(string_mod_4,
           path = "model_4.model")

inits_4 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_neighborhood), 
    beta_1 = 0,
    beta_2 = 0,
    beta_3 = 0,
    tau.y = 1,
    mu.a = 0,
    tau.a = 1
  )
}

parameters_4 <- c("mu.a", "tau.a", "alpha", "beta_1", "beta_2", "beta_3", "tau.y", "yf")

sim_4 <- jags(nyc_sales_list,
              inits_4,
              parameters_4,
              model.file = "model_4.model",
              n.iter = 1000,
              n.chains = 1,
              n.thin = 1,
              n.burnin = 300)

traceplot(sim_4)

# Con 1000 iteraciones tardó 15 segundos

sim_4$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_4 <- sim_4$BUGSoutput$summary %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_sales$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

(preds_4 %>% 
  mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
  .$in_interval %>% 
  sum())/nrow(preds_4)

preds_4 %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res), size = 0.7, alpha = 0.6)

preds_4 %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res), size = 0.7, alpha = 0.6)


preds_4 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_4 %>% 
  sample_n(5000) %>% 
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.6) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.6) +
  geom_abline(slope = 1)

preds_4 %>% 
  sample_n(5000) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(alpha = 0.6) +
  geom_errorbar(aes(ymin = exp(X2.5.), ymax = exp(X97.5.)), alpha = 0.6) +
  geom_abline(slope = 1)


preds_4 %>% 
  mutate(Borough = nyc_sales$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
  geom_abline(slope = 1)

preds_4 %>% 
  mutate(year = nyc_sales$year_cat) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = year), alpha = 0.6) +
  geom_abline(slope = 1)

preds_4 %>% 
  mutate(Building_Class_Description = nyc_sales$Building_Class_Description) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Building_Class_Description), alpha = 0.6) +
  geom_abline(slope = 1)

preds_4 %>% 
  mutate(Building_Class_Description = nyc_sales$Building_Class_Description) %>% 
  mutate(Class2 = substr(Building_Class_Description, 1, 1)) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Class2), alpha = 0.6) +
  geom_abline(slope = 1)


preds_4 %>% 
  mutate(Address = nyc_sales$ADDRESS) %>% 
  filter(log(obs) < 12, mean > 12) %>% 
  View


### frecuentistas

mod2 <- lm(log(SALE_PRICE) ~ log(GROSS_SQUARE_FEET) + Borough + BUILDING_CLASS_CATEGORY*log(GROSS_SQUARE_FEET), 
           data = nyc_sales)

summary(mod2)

tibble(
  yhat = predict(mod2, nyc_sales),
  obs = log(nyc_sales$SALE_PRICE)) %>% 
  ggplot() +
  geom_point(aes(obs, yhat)) +
  geom_abline(slope = 1)

tibble(
  log_res = predict(mod2, nyc_sales) - log(nyc_sales$SALE_PRICE),
  BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY) %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, log_res, color = BUILDING_CLASS_CATEGORY), alpha = 0.7)


tibble(
  res = exp(predict(mod2, nyc_sales)) - nyc_sales$SALE_PRICE,
  BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY) %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res, color = BUILDING_CLASS_CATEGORY), alpha = 0.7)

tibble(
  estimate = exp(predict(mod2, nyc_sales)),
  price = nyc_sales$SALE_PRICE,
  zip_code = nyc_sales$zip_code,
  GROSS_SQUARE_FEET = nyc_sales$GROSS_SQUARE_FEET,
  res = exp(predict(mod2, nyc_sales)) - nyc_sales$SALE_PRICE,
  Address = nyc_sales$ADDRESS,
  BUILDING_CLASS_CATEGORY = nyc_sales$BUILDING_CLASS_CATEGORY) %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% filter(res > 1.5e06 | res < -1.5e06) %>% 
  View


