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

# Algunas gráficas

nyc_sales %>% 
  mutate(Neighborhood2 = paste(Borough, Neighborhood)) %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  facet_wrap(~Neighborhood2, scales = 'free')

nyc_sales %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  facet_wrap(~Borough, scales = 'free')

nyc_sales %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  facet_grid(BUILDING_CLASS_CATEGORY~Borough, scales = 'free')

# nyc_sales %>% 
#   mutate(Neighborhood2 = paste(Borough, Neighborhood)) %>% 
#   ggplot() +
#   geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), alpha = 0.6) +
#   facet_wrap(~Neighborhood2, scales = 'free')


nyc_train %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), alpha = 0.6) +
  facet_wrap(~Neighborhood, scales = 'free')

nyc_test %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), alpha = 0.6) +
  facet_wrap(~Neighborhood, scales = 'free')



nyc_train %>% 
  ungroup() %>% 
  select(GROSS_SQUARE_FEET, SALE_PRICE, Neighborhood) %>% 
  mutate(set = 'train') %>% 
  bind_rows(
    nyc_test %>%
      ungroup() %>% 
      select(GROSS_SQUARE_FEET, SALE_PRICE, Neighborhood) %>% 
      mutate(set = 'test')
  ) %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), alpha = 0.6) +
  facet_grid(set~Neighborhood, scales = 'free')

nyc_train %>% 
  ungroup() %>% 
  select(GROSS_SQUARE_FEET, SALE_PRICE, Borough) %>% 
  mutate(set = 'train') %>% 
  bind_rows(
    nyc_test %>%
      ungroup() %>% 
      select(GROSS_SQUARE_FEET, SALE_PRICE, Borough) %>% 
      mutate(set = 'test')
  ) %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), alpha = 0.6) +
  facet_grid(set~Borough, scales = 'free')

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

# 
# 
# sims <- jags(nyc_sales_list,
#              inits_6,
#              c("mu.a", 
#                "tau.a", 
#                "alpha", 
#                "beta_1", 
#                "beta_2", 
#                "beta_3", 
#                "mu.b1",
#                "tau.b1",
#                "mu.b2",
#                "tau.b2",
#                "mu.b3",
#                "tau.b3",
#                "tau.y",
#                "yf_test"),
#              model.file = "model_6.model",
#              n.iter = 500,
#              n.chains = 1,
#              n.thin = 1,
#              n.burnin = 100)
# 
# 
# sims$BUGSoutput$summary %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>%  
#   slice(grep("test", rowname)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   head(250) %>% 
#   set_names(make.names(names(.))) %>% 
#   ggplot() + 
#   geom_point(aes(x = ix, y = mean)) + 
#   geom_errorbar(aes(x = ix, ymin = X2.5., ymax = X97.5.), alpha = 0.7, size = 0.3) +
#   geom_errorbar(aes(x = ix, ymin = X25., ymax = X75.), alpha = 0.7, size = 0.5) +
#   utilsMBC::axis_labels_vert()
# 
# k = 250
# 
# xf <- tibble(x = head(nyc_sales_list$x_test, k),
#              building_class = head(nyc_sales_list$building_class_test, k),
#              zip_code = head(nyc_sales_list$zip_code_test, k)) 
# 
# bbb <- rep(0.0, sims$BUGSoutput$n.sims)
# 
# for(i in 1:sims$BUGSoutput$n.sims){
#   bbb[i] <- sims$BUGSoutput$sims.list$alpha[i, xf$zip_code[1]] +
#     sims$BUGSoutput$sims.list$beta_1[i, xf$zip_code[1]]*xf$x[1] +
#     sims$BUGSoutput$sims.list$beta_2[i, xf$zip_code[1]]*xf$building_class[1] +
#     sims$BUGSoutput$sims.list$beta_3[i, xf$zip_code[1]]*xf$x[1]*xf$building_class[1]
# }
# 
# rnorm(length(bbb), bbb, sd = 1/sqrt(sims$BUGSoutput$sims.list$tau.y[1])) %>% qplot()
# sims$BUGSoutput$sims.list$yf_test[,1] %>% qplot()
# 
# 
# aaa <- matrix(rep(0.0, nyc_sales_list$n_test*k), nrow = k)
# 
# for(i in 1:nrow(xf)){
#   for(j in 1:sims$BUGSoutput$n.sims){
#     temp <- sims$BUGSoutput$sims.list$alpha[i, xf$zip_code[j]] + 
#       sims$BUGSoutput$sims.list$beta_1[i, xf$zip_code[1:k]]*xf$x[1:k] +
#       sims$BUGSoutput$sims.list$beta_2[i, xf$zip_code[1:k]]*xf$building_class[1:k] +
#       sims$BUGSoutput$sims.list$beta_3[i, xf$zip_code[1:k]]*xf$x[1:k]*xf$building_class[1:k] 
#     aaa[,i] <- rnorm(length(temp), temp, sd = 1/sqrt(sims$BUGSoutput$sims.list$tau.y[1:k])) 
#   }
# }
# 
# aaa_sim <- matrix(rep(0.0, sims$BUGSoutput$n.sims*k), nrow = k)
# 
# for(j in 1:k){
#   aaa_sim[,i] <- rnorm(nrow(aaa), aaa[j,], sd = 1/sqrt(sims$BUGSoutput$sims.list$tau.y[j]))
# }
# 
# rnorm(nrow(aaa), aaa[1,], sd = 1/sqrt(sims$BUGSoutput$sims.list$tau.y[1])) %>% qplot()
# rnorm(nrow(aaa), aaa[2,], sd = 1/sqrt(sims$BUGSoutput$sims.list$tau.y[2])) %>% qplot()
# 
# sims$BUGSoutput$sims.list$yf_test[,1] %>% qplot()
# sims$BUGSoutput$sims.list$yf_test[,2] %>% qplot()
# 
# apply(aaa, 1, function(x) quantile(x, c(0.025, 0.25, 0.5, 0.75, 0.975))) %>% 
#   as.matrix() %>% 
#   t() %>% 
#   as_tibble() %>% 
#   set_names(make.names(names(.))) %>% 
#   mutate(ix = 1:nrow(.)) %>%  
#   ggplot() + 
#   geom_point(aes(x = ix, y = X50.)) + 
#   geom_errorbar(aes(x = ix, ymin = X2.5., ymax = X97.5.), alpha = 0.7, size = 0.3) +
#   geom_errorbar(aes(x = ix, ymin = X25., ymax = X75.), alpha = 0.7, size = 0.5) +
#   utilsMBC::axis_labels_vert()
#  
# # aaa <- (as.matrix(select(simulaciones_1, -sigma))) %*% xf[1,]
# # bbb <- rnorm(nrow(aaa), as.numeric(aaa), simulaciones_1$sigma)
# 
# # Son iguales:
# qplot(bbb)
# sims_yf_1 %>% ggplot() + geom_histogram(aes(xf1))
# 


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

write_file(string_mod_1,
           path = "model_1.model")

inits_1 <- function(){
  list(
    alpha = 0, 
    beta = 0,
    tau = 1
  )
}

parameters_1 <- c("alpha", "beta", "tau", "yf", "yf_test")
# 
# sim_1 <- jags(nyc_sales_list,
#               inits_1,
#               parameters_1,
#               model.file = "model_1.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.burnin = 1000)
# 
# # Con 10000 iteraciones tardó minuto y medio
# 
# saveRDS(sim_1, "../out/models/model_01.rds")

if("summary_mod_1.rds" %in% file_list){
  summary_mod_1 <- read_rds("../out/models/summary_mod_1.rds")
} else {
  summary_mod_1 <- read_rds("../out/models/model_01.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_1, "../out/models/summary_mod_1.rds")
  gc()
}

summary_mod_1 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_1 <- summary_mod_1 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_1 <- summary_mod_1 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

# Percentage of observations inside the 95% probability interval
(preds_1 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_1)

(preds_test_1 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_1)


(rmse_train_1 <- sqrt(mean(preds_1$res^2)))
(rmse_train_log_1 <- sqrt(mean((preds_1$mean - log(preds_1$obs))^2)))
(rmse_test_1 <- sqrt(mean(preds_test_1$res^2)))
(rmse_test_log_1 <- sqrt(mean((preds_test_1$mean - log(preds_test_1$obs))^2)))

preds_1 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)

preds_test_1 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


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

  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }

  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[borough_test[i]] + beta*x_test[i]
  }
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

parameters_2 <- c("mu.a", "tau.a", "alpha", "beta", "tau.y", "yf", "yf_test")

# sim_2 <- jags(nyc_sales_list,
#               inits_2,
#               parameters_2,
#               model.file = "model_2.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.thin = 2,
#               n.burnin = 2000)
# 
# saveRDS(sim_2, "../out/models/model_02.rds")


if("summary_mod_2.rds" %in% file_list){
  summary_mod_2 <- read_rds("../out/models/summary_mod_2.rds")
} else {
  summary_mod_2 <- read_rds("../out/models/model_02.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_2, "../out/models/summary_mod_2.rds")
  gc()
}

#traceplot(sim_2)

# Para 20000 iteraciones tardó 3 minutos

summary_mod_2 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_2 <- summary_mod_2 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_2 <- summary_mod_2 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


# Percentage of observations inside the 95% probability interval
(preds_2 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_2)

(preds_test_2 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_2)


(rmse_train_2 <- sqrt(mean(preds_2$res^2)))
(rmse_train_log_2 <- sqrt(mean((preds_2$mean - log(preds_2$obs))^2)))
(rmse_test_2 <- sqrt(mean(preds_test_2$res^2)))
(rmse_test_log_2 <- sqrt(mean((preds_test_2$mean - log(preds_test_2$obs))^2)))

preds_2 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)

preds_test_2 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)





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

  # Predictions test
  for(i in 1:n){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[neighborhood_test[i]] + beta*x_test[i]
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

# sim_3 <- jags(nyc_sales_list,
#               inits_3,
#               parameters_3,
#               model.file = "model_3.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.thin = 2,
#               n.burnin = 2000)
# saveRDS(sim_3, "../out/models/model_03.rds")
# Con 1000 iteraciones tardó 15 segundos

if("summary_mod_3.rds" %in% file_list){
  summary_mod_3 <- read_rds("../out/models/summary_mod_3.rds")
} else {
  summary_mod_3 <- read_rds("../out/models/model_03.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_3, "../out/models/summary_mod_3.rds")
  gc()
}


preds_3 <- summary_mod_3 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_3 <- summary_mod_3 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  select(mean, X2.5., X97.5.) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
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
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY,
         Borough = nyc_train$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_3 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY,
         Borough = nyc_train$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
  geom_abline(slope = 1)

preds_3 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY,
         Borough = nyc_train$Borough,
         year = nyc_train$year_cat) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = year), alpha = 0.6) +
  geom_abline(slope = 1)

preds_3 %>% 
  mutate(Address = nyc_train$ADDRESS) %>% 
  filter(log(obs) < 12, mean > 12) %>% 
  View


# 
# ## Modelo 4: Jerárquico con vecindarios y tipo de edificio como covariable
# 
# string_mod_4 <- "model {
#   for(i in 1 : n) {
#     y[i] ~ dnorm(mu[i], tau.y) 
#     mu[i] <- alpha[neighborhood[i]] + beta_1*x[i] + beta_2*building_class[i] + 
#   beta_3*building_class[i]*x[i]
#   }
#   beta_1 ~ dnorm(0, 0.0001)
#   beta_2 ~ dnorm(0, 0.0001)
#   beta_3 ~ dnorm(0, 0.0001)
#   tau.y ~ dgamma(0.001, 0.001)
#   for(j in 1:n_neighborhood){
#     alpha[j] ~ dnorm(mu.a, tau.a)
#   }
#   mu.a ~ dnorm(0, 0.0001)
#   tau.a ~ dgamma(0.001, 0.001)
# 
#   # Predictions
# 
#   for(i in 1:n){
#     yf[i] ~ dnorm(mu[i], tau.y) 
#   }
# }"
# 
# write_file(string_mod_4,
#            path = "model_4.model")
# 
# inits_4 <- function(){
#   list(
#     alpha = rep(0, nyc_sales_list$n_neighborhood), 
#     beta_1 = 0,
#     beta_2 = 0,
#     beta_3 = 0,
#     tau.y = 1,
#     mu.a = 0,
#     tau.a = 1
#   )
# }
# 
# parameters_4 <- c("mu.a", "tau.a", "alpha", "beta_1", "beta_2", "beta_3", "tau.y", "yf")
# 
# sim_4 <- jags(nyc_sales_list,
#               inits_4,
#               parameters_4,
#               model.file = "model_4.model",
#               n.iter = 1000,
#               n.chains = 1,
#               n.thin = 1,
#               n.burnin = 300)
# 
# traceplot(sim_4)
# 
# # Con 1000 iteraciones tardó 15 segundos
# 
# sim_4$BUGSoutput$summary %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   filter(!grepl("yf", rowname))
# 
# preds_4 <- sim_4$BUGSoutput$summary %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   slice(grep("yf", rowname)) %>% 
#   set_names(make.names(names(.))) %>% 
#   select(mean, X2.5., X97.5.) %>% 
#   mutate(obs = nyc_train$SALE_PRICE,
#          adj = exp(mean)) %>% 
#   mutate(res = obs - exp(mean))
# 
# (preds_4 %>% 
#     mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
#     .$in_interval %>% 
#     sum())/nrow(preds_4)
# 
# preds_4 %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, res), size = 0.7, alpha = 0.6)
# 
# preds_4 %>% 
#   sample_n(nrow(.)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, res), size = 0.7, alpha = 0.6)
# 
# 
# preds_4 %>% 
#   mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# 
# preds_4 %>% 
#   sample_n(5000) %>% 
#   ggplot(aes(log(obs), mean)) +
#   geom_point(alpha = 0.6) +
#   geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# preds_4 %>% 
#   sample_n(5000) %>% 
#   ggplot(aes(obs, adj)) +
#   geom_point(alpha = 0.6) +
#   geom_errorbar(aes(ymin = exp(X2.5.), ymax = exp(X97.5.)), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# 
# preds_4 %>% 
#   mutate(Borough = nyc_train$Borough) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# preds_4 %>% 
#   mutate(year = nyc_train$year_cat) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = year), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# preds_4 %>% 
#   mutate(Building_Class_Description = nyc_train$Building_Class_Description) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = Building_Class_Description), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# preds_4 %>% 
#   mutate(Building_Class_Description = nyc_train$Building_Class_Description) %>% 
#   mutate(Class2 = substr(Building_Class_Description, 1, 1)) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = Class2), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# 
# preds_4 %>% 
#   mutate(Address = nyc_train$ADDRESS) %>% 
#   filter(log(obs) < 12, mean > 12) %>% 
#   View
# 
# 
# 
# 
# ## Modelo 5: Jerárquico con zip codes y tipo de edificio como covariable
# 
# string_mod_5 <- "model {
#   for(i in 1 : n) {
#     y[i] ~ dnorm(mu[i], tau.y) 
#     mu[i] <- alpha[zip_code[i]] + beta_1*x[i] + beta_2*building_class[i] + 
      beta_3*building_class[i]*x[i]
#   }
#   beta_1 ~ dnorm(0, 0.0001)
#   beta_2 ~ dnorm(0, 0.0001)
#   beta_3 ~ dnorm(0, 0.0001)
#   tau.y ~ dgamma(0.001, 0.001)
#   for(j in 1:n_zip){
#     alpha[j] ~ dnorm(mu.a, tau.a)
#   }
#   mu.a ~ dnorm(0, 0.0001)
#   tau.a ~ dgamma(0.001, 0.001)
# 
#   # Predictions
# 
#   for(i in 1:n){
#     yf[i] ~ dnorm(mu[i], tau.y) 
#   }
# }"
# 
# write_file(string_mod_5,
#            path = "model_5.model")
# 
# inits_5 <- function(){
#   list(
#     alpha = rep(0, nyc_sales_list$n_zip), 
#     beta_1 = 0,
#     beta_2 = 0,
#     beta_3 = 0,
#     tau.y = 1,
#     mu.a = 0,
#     tau.a = 1
#   )
# }
# 
# parameters_5 <- c("mu.a", "tau.a", "alpha", "beta_1", "beta_2", "beta_3", "tau.y", "yf")
# 
# sim_5 <- jags(nyc_sales_list,
#               inits_5,
#               parameters_5,
#               model.file = "model_5.model",
#               n.iter = 1000,
#               n.chains = 1,
#               n.thin = 1,
#               n.burnin = 300)
# 
# traceplot(sim_5)
# 
# # Con 1000 iteraciones tardó 91 segundos
# 
# sim_5$BUGSoutput$summary %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   filter(!grepl("yf", rowname))
# 
# preds_5 <- sim_5$BUGSoutput$summary %>% 
#   as.data.frame() %>% 
#   rownames_to_column() %>% 
#   slice(grep("yf", rowname)) %>% 
#   set_names(make.names(names(.))) %>% 
#   select(mean, X2.5., X97.5.) %>% 
#   mutate(obs = nyc_train$SALE_PRICE,
#          adj = exp(mean)) %>% 
#   mutate(res = obs - exp(mean))
# 
# (rmse_train_5 <- mean(preds_5$res^2))
# (rmse_train_log_5 <- mean((preds_5$mean - log(preds_5$obs))^2))
# 
# 
# (preds_5 %>% 
#     mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
#     .$in_interval %>% 
#     sum())/nrow(preds_5)
# 
# preds_5 %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, res), size = 0.7, alpha = 0.6)
# 
# preds_5 %>% 
#   sample_n(nrow(.)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, res), size = 0.7, alpha = 0.6)
# 
# preds_5 %>% 
#   mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   sample_n(nrow(.)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, res, color = BUILDING_CLASS_CATEGORY), size = 0.8, alpha = 0.6)
# 
# 
# preds_5 %>% 
#   mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# 
# preds_5 %>% 
#   sample_n(6000) %>% 
#   ggplot(aes(log(obs), mean)) +
#   geom_point(alpha = 0.4, size = 0.4) +
#   geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
#   geom_abline(slope = 1)
# 
# preds_5 %>% 
#   sample_n(5000) %>% 
#   ggplot(aes(obs, adj)) +
#   geom_point(alpha = 0.4) +
#   geom_errorbar(aes(ymin = exp(X2.5.), ymax = exp(X97.5.)), alpha = 0.4) +
#   geom_abline(slope = 1)
# 
# preds_5 %>% 
#   mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   ggplot(aes(obs, adj)) +
#   geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
#   geom_abline(slope = 1)
# 
# 
# preds_5 %>% 
#   mutate(Borough = nyc_train$Borough) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
#   geom_abline(slope = 1)




## Modelo 6: Jerárquico con zip codes, tipo de edificio como covariable 
## y pendientes e interceptos distintos

string_mod_6 <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <-  alpha[zip_code[i]] + 
              beta_1[zip_code[i]]*x[i] + 
              beta_2[zip_code[i]]*building_class[i] + 
              beta_3[zip_code[i]]*building_class[i]*x[i]
  }
  tau.y ~ dgamma(0.001, 0.001)
  for(j in 1:n_zip){
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
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }

  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[zip_code_test[i]] + 
                  beta_1[zip_code_test[i]]*x_test[i] + 
                  beta_2[zip_code_test[i]]*building_class_test[i] + 
                  beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
  }

}"

write_file(string_mod_6,
           path = "model_6.model")

inits_6 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
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

# sim_6 <- jags(nyc_sales_list,
#               inits_6,
#               parameters_6,
#               model.file = "model_6.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.thin = 2,
#               n.burnin = 2000)
#
# saveRDS(sim_6, "../out/models/model_06.rds")


if("summary_mod_6.rds" %in% file_list){
  summary_mod_6 <- read_rds("../out/models/summary_mod_6.rds")
} else {
  summary_mod_6 <- read_rds("../out/models/model_06.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_6, "../out/models/summary_mod_6.rds")
  gc()
}

#traceplot(sim_6)

# Con 1000 iteraciones tardó 91 segundos

summary_mod_6 %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  filter(!grepl("yf", rowname))

preds_6 <- summary_mod_6 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_6 <- summary_mod_6 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))




(rmse_train_6 <- sqrt(mean(preds_6$res^2)))
(rmse_train_log_6 <- sqrt(mean((preds_6$mean - log(preds_6$obs))^2)))
(rmse_test_6 <- sqrt(mean(preds_test_6$res^2)))
(rmse_test_log_6 <- sqrt(mean((preds_test_6$mean - log(preds_test_6$obs))^2)))

(preds_6 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_6)

(preds_6 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_6)


(preds_test_6 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_6)


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


preds_test_6 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)



preds_6 %>% 
  sample_n(6000) %>% 
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.4, size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
  geom_abline(slope = 1)


preds_test_6 %>% 
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






## Modelo 7: Jerárquico con vecindarios, tipo de edificio como covariable 
# y pendientes e interceptos distintos

string_mod_7 <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <-  alpha[neighborhood[i]] + 
              beta_1[neighborhood[i]]*x[i] + 
              beta_2[neighborhood[i]]*building_class[i] + 
              beta_3[neighborhood[i]]*building_class[i]*x[i]
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
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }
  
  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[neighborhood_test[i]] + 
      beta_1[neighborhood_test[i]]*x_test[i] + 
      beta_2[neighborhood_test[i]]*building_class_test[i] + 
      beta_3[neighborhood_test[i]]*building_class_test[i]*x_test[i]
  }

}"

write_file(string_mod_7,
           path = "model_7.model")

inits_7 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_neighborhood), 
    beta_1 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_2 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_3 = rep(0, nyc_sales_list$n_neighborhood), 
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

parameters_7 <- c("mu.a", 
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

# sim_7 <- jags(nyc_sales_list,
#               inits_7,
#               parameters_7,
#               model.file = "model_7.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.thin = 2,
#               n.burnin = 2000)
# 
# saveRDS(sim_7, "../out/models/model_07.rds")


#sim_7 <- read_rds("../out/models/model_07.rds")

if("summary_mod_7.rds" %in% file_list){
  summary_mod_7 <- read_rds("../out/models/summary_mod_7.rds")
} else {
  summary_mod_7 <- read_rds("../out/models/model_07.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_7, "../out/models/summary_mod_7.rds")
  gc()
}

summary_mod_7 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_7 <- summary_mod_7 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_7 <- summary_mod_7 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_7 <- sqrt(mean(preds_7$res^2)))
(rmse_train_log_7 <- sqrt(mean((preds_7$mean - log(preds_7$obs))^2)))
(rmse_test_7 <- sqrt(mean(preds_test_7$res^2)))
(rmse_test_log_7 <- sqrt(mean((preds_test_7$mean - log(preds_test_7$obs))^2)))

(preds_7 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_7)

(preds_7 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_7)


(preds_test_7 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_7)


(preds_test_7 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_7)


preds_7 %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res), size = 0.7, alpha = 0.6)

preds_test_7 %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res), size = 0.7, alpha = 0.6)


preds_7 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res, color = BUILDING_CLASS_CATEGORY), size = 0.8, alpha = 0.6)

preds_test_7 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  sample_n(nrow(.)) %>% 
  mutate(ix = 1:nrow(.)) %>% 
  ggplot() +
  geom_point(aes(ix, res, color = BUILDING_CLASS_CATEGORY), size = 1, alpha = 0.8)


preds_7 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_7 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)



preds_7 %>% 
  sample_n(6000) %>% 
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.4, size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
  geom_abline(slope = 1)


preds_test_7 %>% 
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.4, size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
  geom_abline(slope = 1)

preds_7 %>% 
  sample_n(5000) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(alpha = 0.4) +
  geom_errorbar(aes(ymin = exp(X2.5.), ymax = exp(X97.5.)), alpha = 0.4) +
  geom_abline(slope = 1)

preds_7 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(obs, adj)) +
  geom_point(aes(color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_7 %>% 
  mutate(Borough = nyc_train$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_7 %>% 
  mutate(Borough = nyc_test$Borough) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = Borough), alpha = 0.6) +
  geom_abline(slope = 1)



preds_7 %>% 
  mutate(Neighborhood = nyc_train$Neighborhood) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

preds_7 %>% 
  mutate(Neighborhood = paste(nyc_train$Borough, nyc_train$Neighborhood)) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean)), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

preds_7 %>% 
  mutate(Neighborhood = paste(nyc_train$Borough, nyc_train$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)



preds_test_7 %>% 
  mutate(Neighborhood = nyc_test$Neighborhood) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean)), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

preds_test_7 %>% 
  mutate(Neighborhood = paste(nyc_test$Borough, nyc_test$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)


nyc_sales %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE), 
                 color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  facet_grid(BUILDING_CLASS_CATEGORY~Borough, scales = 'free')



nyc_train %>% 
  ungroup() %>% 
  select(GROSS_SQUARE_FEET, SALE_PRICE, Neighborhood) %>% 
  mutate(set = 'train') %>% 
  bind_rows(
    nyc_test %>%
      ungroup() %>% 
      select(GROSS_SQUARE_FEET, SALE_PRICE, Neighborhood) %>% 
      mutate(set = 'test')
  ) %>% 
  filter(Neighborhood == "Southeast Bronx") %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE), color = set), alpha = 0.6) 






## Modelo 8: Jerárquico con vecindarios, tipo de edificio como covariable y 
# pendientes, interceptos y varianzas diferentes

string_mod_8 <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y[neighborhood[i]]) 
    mu[i] <-  alpha[neighborhood[i]] + 
              beta_1[neighborhood[i]]*x[i] + 
              beta_2[neighborhood[i]]*building_class[i] + 
              beta_3[neighborhood[i]]*building_class[i]*x[i]
  }
  for(j in 1:n_neighborhood){
    tau.y[j] ~ dgamma(tau.y.h1, tau.y.h2)
    alpha[j] ~ dnorm(mu.a, tau.a)
    beta_1[j] ~ dnorm(mu.b1, tau.b1)
    beta_2[j] ~ dnorm(mu.b2, tau.b2)
    beta_3[j] ~ dnorm(mu.b3, tau.b3)
  }
  tau.y.h1 ~ dgamma(0.001, 0.001)
  tau.y.h2 ~ dgamma(0.001, 0.001)
  mu.a ~ dnorm(0, 0.0001)
  tau.a ~ dgamma(0.001, 0.001)
  mu.b1 ~ dnorm(0, 0.0001)
  tau.b1 ~ dgamma(0.001, 0.001)
  mu.b2 ~ dnorm(0, 0.0001)
  tau.b2 ~ dgamma(0.001, 0.001)
  mu.b3 ~ dnorm(0, 0.0001)
  tau.b3 ~ dgamma(0.001, 0.001)
  
  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y[neighborhood[i]]) 
  }
  
  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y[neighborhood_test[i]])
    mu_test[i] <- alpha[neighborhood_test[i]] + 
                  beta_1[neighborhood_test[i]]*x_test[i] + 
                  beta_2[neighborhood_test[i]]*building_class_test[i] + 
                  beta_3[neighborhood_test[i]]*building_class_test[i]*x_test[i]
  }
}"

write_file(string_mod_8,
           path = "model_8.model")

inits_8 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_neighborhood), 
    beta_1 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_2 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_3 = rep(0, nyc_sales_list$n_neighborhood), 
    tau.y = rep(1, nyc_sales_list$n_neighborhood), 
    tau.y.h1 = 1,
    tau.y.h2 = 1,
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

parameters_8 <- c("mu.a", 
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
                  "tau.y.h1",
                  "tau.y.h2",
                  "yf",
                  "yf_test")

# sim_8 <- jags(nyc_sales_list,
#               inits_8,
#               parameters_8,
#               model.file = "model_8.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.thin = 2,
#               n.burnin = 2000)
# 
# saveRDS(sim_8, "../out/models/model_08.rds")


#sim_8 <- read_rds("../out/models/model_08.rds")

if("summary_mod_8.rds" %in% file_list){
  summary_mod_8 <- read_rds("../out/models/summary_mod_8.rds")
} else {
  summary_mod_8 <- read_rds("../out/models/model_08.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_8, "../out/models/summary_mod_8.rds")
  gc()
}

summary_mod_8 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_8 <- summary_mod_8 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_8 <- summary_mod_8 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_8 <- sqrt(mean(preds_8$res^2)))
(rmse_train_log_8 <- sqrt(mean((preds_8$mean - log(preds_8$obs))^2)))
(rmse_test_8 <- sqrt(mean(preds_test_8$res^2)))
(rmse_test_log_8 <- sqrt(mean((preds_test_8$mean - log(preds_test_8$obs))^2)))

(preds_8 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_8)

(preds_8 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_8)


(preds_test_8 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_8)


(preds_test_8 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_8)

preds_8 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_8 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)



## Modelo 9: Jerárquico con zip codes, tipo de edificio como covariable y 
### pendientes, interceptos y varianzas diferentes

string_mod_9 <- "model {
for(i in 1:n) {
y[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
mu[i] <-  alpha[zip_code[i]] + 
beta_1[zip_code[i]]*x[i] + 
beta_2[zip_code[i]]*building_class[i] + 
beta_3[zip_code[i]]*building_class[i]*x[i]
}
for(j in 1:n_zip){
tau.y[j] ~ dgamma(tau.y.h1, tau.y.h2)
alpha[j] ~ dnorm(mu.a, tau.a)
beta_1[j] ~ dnorm(mu.b1, tau.b1)
beta_2[j] ~ dnorm(mu.b2, tau.b2)
beta_3[j] ~ dnorm(mu.b3, tau.b3)
}
tau.y.h1 ~ dgamma(0.001, 0.001)
tau.y.h2 ~ dgamma(0.001, 0.001)
mu.a ~ dnorm(0, 0.0001)
tau.a ~ dgamma(0.001, 0.001)
mu.b1 ~ dnorm(0, 0.0001)
tau.b1 ~ dgamma(0.001, 0.001)
mu.b2 ~ dnorm(0, 0.0001)
tau.b2 ~ dgamma(0.001, 0.001)
mu.b3 ~ dnorm(0, 0.0001)
tau.b3 ~ dgamma(0.001, 0.001)

# Train predictions
for(i in 1:n){
yf[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
}

# Test predictions
for(i in 1:n_test){
yf_test[i] ~ dnorm(mu_test[i], tau.y[zip_code_test[i]])
mu_test[i] <- alpha[zip_code_test[i]] + 
beta_1[zip_code_test[i]]*x_test[i] + 
beta_2[zip_code_test[i]]*building_class_test[i] + 
beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
}
}"

write_file(string_mod_9,
           path = "model_9.model")

inits_9 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
    tau.y = rep(1, nyc_sales_list$n_zip), 
    tau.y.h1 = 1,
    tau.y.h2 = 1,
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

parameters_9 <- c("mu.a", 
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
                  "tau.y.h1",
                  "tau.y.h2",
                  "yf",
                  "yf_test")

# sim_9 <- jags(nyc_sales_list,
#               inits_9,
#               parameters_9,
#               model.file = "model_9.model",
#               n.iter = 10000,
#               n.chains = 1,
#               n.thin = 2,
#               n.burnin = 2000)
# 
# saveRDS(sim_9, "../out/models/model_09.rds")


#sim_9 <- read_rds("../out/models/model_09.rds")

if("summary_mod_9.rds" %in% file_list){
  summary_mod_9 <- read_rds("../out/models/summary_mod_9.rds")
} else {
  summary_mod_9 <- read_rds("../out/models/model_09.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_9, "../out/models/summary_mod_9.rds")
  gc()
}

summary_mod_9 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_9 <- summary_mod_9 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_9 <- summary_mod_9 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_9 <- sqrt(mean(preds_9$res^2)))
(rmse_train_log_9 <- sqrt(mean((preds_9$mean - log(preds_9$obs))^2)))
(rmse_test_9 <- sqrt(mean(preds_test_9$res^2)))
(rmse_test_log_9 <- sqrt(mean((preds_test_9$mean - log(preds_test_9$obs))^2)))

(preds_9 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_9)

(preds_9 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_9)

(preds_test_9 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_9)

(preds_test_9 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_9)

preds_9 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_9 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)

preds_9 %>% 
  mutate(zip_code = nyc_train$zip_code,
         BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~zip_code)

preds_test_9 %>% 
  mutate(zip_code = nyc_test$zip_code,
         BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~zip_code)

preds_9 %>% 
  mutate(Neighborhood = paste(nyc_train$Borough, nyc_train$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

preds_test_9 %>% 
  mutate(Neighborhood = paste(nyc_test$Borough, nyc_test$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)



summary_mod_9 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("beta_1", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  ggplot() + 
  geom_point(aes(rowname, X50.)) + 
  geom_errorbar(aes(x = rowname, ymin = X2.5., ymax = X97.5.))


## Más niveles

#### Modelo 11: Jerárquico con vecinadirios y distritos, tipo de edificio como covariable y 
### pendientes e interceptos distintos


string_mod_11 <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <-  alpha[neighborhood[i]] + 
              beta_1[neighborhood[i]]*x[i] + 
              beta_2[neighborhood[i]]*building_class[i] + 
              beta_3[neighborhood[i]]*building_class[i]*x[i]
  }
  tau.y ~ dgamma(0.001, 0.001)

  for(j in 1:n_neighborhood){
    alpha[j] ~ dnorm(mu.a[borough[j]], tau.a[borough[j]])
    beta_1[j] ~ dnorm(mu.b1[borough[j]], tau.b1[borough[j]])
    beta_2[j] ~ dnorm(mu.b2[borough[j]], tau.b2[borough[j]])
    beta_3[j] ~ dnorm(mu.b3[borough[j]], tau.b3[borough[j]])
  }

  for(z in 1:n_borough){
    mu.a[z] ~ dnorm(mu.0.a, tau.a.0)
    mu.b1[z] ~ dnorm(mu.0.b1, tau.0.b1)
    mu.b2[z] ~ dnorm(mu.0.b2, tau.0.b2)
    mu.b3[z] ~ dnorm(mu.0.b3, tau.0.b3)
    tau.a[z] ~ dexp(lambda.a)
    tau.b1[z] ~dexp(lambda.b1)
    tau.b2[z] ~dexp(lambda.b2)
    tau.b3[z] ~ dexp(lambda.b3)
  }

  mu.0.a ~ dnorm(0, 0.0001)
  lambda.a ~ dgamma(0.001, 0.001)
  mu.0.b1 ~ dnorm(0, 0.0001)
  lambda.b1 ~ dgamma(0.001, 0.001)
  mu.0.b2 ~ dnorm(0, 0.0001)
  lambda.b2 ~ dgamma(0.001, 0.001)
  mu.0.b3 ~ dnorm(0, 0.0001)
  lambda.b3 ~ dgamma(0.001, 0.001)
  tau.a.0 ~ dgamma(0.001, 0.001)
  tau.0.b1 ~ dgamma(0.001, 0.001)
  tau.0.b2 ~ dgamma(0.001, 0.001)
  tau.0.b3 ~ dgamma(0.001, 0.001)

# Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }

  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[neighborhood_test[i]] + 
                  beta_1[neighborhood_test[i]]*x_test[i] + 
                  beta_2[neighborhood_test[i]]*building_class_test[i] + 
                  beta_3[neighborhood_test[i]]*building_class_test[i]*x_test[i]
  }

}"

write_file(string_mod_11,
           path = "model_11.model")

inits_11 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_neighborhood), 
    beta_1 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_2 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_3 = rep(0, nyc_sales_list$n_neighborhood), 
    mu.a = rep(0, nyc_sales_list$n_borough),
    mu.b1 = rep(0, nyc_sales_list$n_borough),
    mu.b2 = rep(0, nyc_sales_list$n_borough),
    mu.b3 = rep(0, nyc_sales_list$n_borough),
    tau.a = rep(1, nyc_sales_list$n_borough),
    tau.b1 = rep(1, nyc_sales_list$n_borough),
    tau.b2 = rep(1, nyc_sales_list$n_borough),
    tau.b3 = rep(1, nyc_sales_list$n_borough),
    mu.0.a = 0,
    lambda.a =1,
    mu.0.b1 = 0,
    lambda.b1 =1,
    mu.0.b2 = 0,
    lambda.b2 =1,
    mu.0.b3 = 0,
    lambda.b3 =1,
    tau.a.0 = 1,
    tau.0.b1 = 1,
    tau.0.b2 = 1,
    tau.0.b3 = 1
  )
}

parameters_11 <- c("mu.a", 
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
                   "mu.0.a",
                   "lambda.a",
                   "mu.0.b1",
                   "lambda.b1",
                   "mu.0.b2",
                   "lambda.b2",
                   "mu.0.b3 ",
                   "lambda.b3 ",
                   "tau.a.0 ",
                   "tau.0.b1",
                   "tau.0.b2",
                   "tau.0.b3",
                   "yf",
                   "yf_test"
                   
)

# sim_11 <- jags(nyc_sales_list,
#                inits_11,
#                parameters_11,
#                model.file = "model_11.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_11, "../out/models/model_11.rds")


if("summary_mod_11.rds" %in% file_list){
  summary_mod_11 <- read_rds("../out/models/summary_mod_11.rds")
} else {
  summary_mod_11 <- read_rds("../out/models/model_11.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_11, "../out/models/summary_mod_11.rds")
  gc()
}

summary_mod_11 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_11 <- summary_mod_11 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_11 <- summary_mod_11 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_11 <- sqrt(mean(preds_11$res^2)))
(rmse_train_log_11 <- sqrt(mean((preds_11$mean - log(preds_11$obs))^2)))
(rmse_test_11 <- sqrt(mean(preds_test_11$res^2)))
(rmse_test_log_11 <- sqrt(mean((preds_test_11$mean - log(preds_test_11$obs))^2)))

(preds_11 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_11)

(preds_11 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_11)

(preds_test_11 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_11)

(preds_test_11 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_11)



preds_11 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_11 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)






#### Modelo 12: Igual que el 11 pero con gammas en lugar de exponenciales


string_mod_12 <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <-  alpha[neighborhood[i]] + 
              beta_1[neighborhood[i]]*x[i] + 
              beta_2[neighborhood[i]]*building_class[i] + 
              beta_3[neighborhood[i]]*building_class[i]*x[i]
  }
  tau.y ~ dgamma(0.1, 0.1)

  for(j in 1:n_neighborhood){
    alpha[j] ~ dnorm(mu.a[borough[j]], tau.a[borough[j]])
    beta_1[j] ~ dnorm(mu.b1[borough[j]], tau.b1[borough[j]])
    beta_2[j] ~ dnorm(mu.b2[borough[j]], tau.b2[borough[j]])
    beta_3[j] ~ dnorm(mu.b3[borough[j]], tau.b3[borough[j]])
  }

  for(z in 1:n_borough){
    mu.a[z] ~ dnorm(mu.0.a, tau.a.0)
    mu.b1[z] ~ dnorm(mu.0.b1, tau.0.b1)
    mu.b2[z] ~ dnorm(mu.0.b2, tau.0.b2)
    mu.b3[z] ~ dnorm(mu.0.b3, tau.0.b3)
    tau.a[z] ~ dgamma(alpha.a, beta.a)
    tau.b1[z] ~ dgamma(alpha.b1, beta.b1)
    tau.b2[z] ~ dgamma(alpha.b2, beta.b2)
    tau.b3[z] ~ dgamma(alpha.b3, beta.b3)
  }

  mu.0.a ~ dnorm(0, 0.0001)
  alpha.a ~ dgamma(0.1, 0.1)
  beta.a ~ dgamma(0.1, 0.1)
  mu.0.b1 ~ dnorm(0, 0.0001)
  alpha.b1 ~ dgamma(0.1, 0.1)
  beta.b1 ~ dgamma(0.1, 0.1)
  mu.0.b2 ~ dnorm(0, 0.0001)
  alpha.b2 ~ dgamma(0.1, 0.1)
  beta.b2 ~ dgamma(0.1, 0.1)
  mu.0.b3 ~ dnorm(0, 0.0001)
  alpha.b3 ~ dgamma(0.1, 0.1)
  beta.b3 ~ dgamma(0.1, 0.1)
  tau.a.0 ~ dgamma(0.1, 0.1)
  tau.0.b1 ~ dgamma(0.1, 0.1)
  tau.0.b2 ~ dgamma(0.1, 0.1)
  tau.0.b3 ~ dgamma(0.1, 0.1)

  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }

  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[neighborhood_test[i]] + 
                  beta_1[neighborhood_test[i]]*x_test[i] + 
                  beta_2[neighborhood_test[i]]*building_class_test[i] + 
                  beta_3[neighborhood_test[i]]*building_class_test[i]*x_test[i]
  }

}"

write_file(string_mod_12,
           path = "model_12.model")

inits_12 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_neighborhood), 
    beta_1 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_2 = rep(0, nyc_sales_list$n_neighborhood), 
    beta_3 = rep(0, nyc_sales_list$n_neighborhood), 
    mu.a = rep(0, nyc_sales_list$n_borough),
    mu.b1 = rep(0, nyc_sales_list$n_borough),
    mu.b2 = rep(0, nyc_sales_list$n_borough),
    mu.b3 = rep(0, nyc_sales_list$n_borough),
    tau.a = rep(1, nyc_sales_list$n_borough),
    tau.b1 = rep(1, nyc_sales_list$n_borough),
    tau.b2 = rep(1, nyc_sales_list$n_borough),
    tau.b3 = rep(1, nyc_sales_list$n_borough),
    mu.0.a = 0,
    alpha.a = 1,
    beta.a = 1,
    mu.0.b1 = 0,
    alpha.b1 = 1,
    beta.b1 = 1,
    mu.0.b2 = 0,
    alpha.b2 = 1,
    beta.b2 = 1,
    mu.0.b3 = 0,
    alpha.b3 = 1,
    beta.b3 = 1,
    tau.a.0 = 1,
    tau.0.b1 = 1,
    tau.0.b2 = 1,
    tau.0.b3 = 1,
    tau.y = 1
  )
}

parameters_12 <- c("mu.a", 
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
                   "mu.0.a",
                   "beta.a",
                   "alpha.a",
                   "mu.0.b1",
                   "alpha.b1",
                   "beta.b1",
                   "mu.0.b2",
                   "alpha.b2",
                   "beta.b2",
                   "mu.0.b3 ",
                   "alpha.b3",
                   "beta.b3",
                   "tau.a.0 ",
                   "tau.0.b1",
                   "tau.0.b2",
                   "tau.0.b3",
                   "yf",
                   "yf_test"
                   
)

# sim_12 <- jags(nyc_sales_list,
#                inits_12,
#                parameters_12,
#                model.file = "model_12.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_12, "../out/models/model_12.rds")


if("summary_mod_12.rds" %in% file_list){
  summary_mod_12 <- read_rds("../out/models/summary_mod_12.rds")
} else {
  summary_mod_12 <- read_rds("../out/models/model_12.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_12, "../out/models/summary_mod_12.rds")
  gc()
}

summary_mod_12 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_12 <- summary_mod_12 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_12 <- summary_mod_12 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_12 <- sqrt(mean(preds_12$res^2)))
(rmse_train_log_12 <- sqrt(mean((preds_12$mean - log(preds_12$obs))^2)))
(rmse_test_12 <- sqrt(mean(preds_test_12$res^2)))
(rmse_test_log_12 <- sqrt(mean((preds_test_12$mean - log(preds_test_12$obs))^2)))

(preds_12 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_12)

(preds_12 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_12)

(preds_test_12 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_12)

(preds_test_12 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_12)



preds_12 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_12 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)














#### Modelo 13: Igual que el 11 pero con zip code en lugar de neighborhoods


string_mod_13 <- "model {
  for(i in 1:n) {
    y[i] ~ dnorm(mu[i], tau.y) 
    mu[i] <-  alpha[zip_code[i]] + 
              beta_1[zip_code[i]]*x[i] + 
              beta_2[zip_code[i]]*building_class[i] + 
              beta_3[zip_code[i]]*building_class[i]*x[i]
  }
  tau.y ~ dgamma(0.001, 0.001)
  
  for(j in 1:n_zip){
    alpha[j] ~ dnorm(mu.a[borough[j]], tau.a[borough[j]])
    beta_1[j] ~ dnorm(mu.b1[borough[j]], tau.b1[borough[j]])
    beta_2[j] ~ dnorm(mu.b2[borough[j]], tau.b2[borough[j]])
    beta_3[j] ~ dnorm(mu.b3[borough[j]], tau.b3[borough[j]])
  }
  
  for(z in 1:n_borough){
    mu.a[z] ~ dnorm(mu.0.a, tau.a.0)
    mu.b1[z] ~ dnorm(mu.0.b1, tau.0.b1)
    mu.b2[z] ~ dnorm(mu.0.b2, tau.0.b2)
    mu.b3[z] ~ dnorm(mu.0.b3, tau.0.b3)
    tau.a[z] ~ dexp(lambda.a)
    tau.b1[z] ~dexp(lambda.b1)
    tau.b2[z] ~dexp(lambda.b2)
    tau.b3[z] ~ dexp(lambda.b3)
  }
  
  mu.0.a ~ dnorm(0, 0.0001)
  lambda.a ~ dgamma(0.001, 0.001)
  mu.0.b1 ~ dnorm(0, 0.0001)
  lambda.b1 ~ dgamma(0.001, 0.001)
  mu.0.b2 ~ dnorm(0, 0.0001)
  lambda.b2 ~ dgamma(0.001, 0.001)
  mu.0.b3 ~ dnorm(0, 0.0001)
  lambda.b3 ~ dgamma(0.001, 0.001)
  tau.a.0 ~ dgamma(0.001, 0.001)
  tau.0.b1 ~ dgamma(0.001, 0.001)
  tau.0.b2 ~ dgamma(0.001, 0.001)
  tau.0.b3 ~ dgamma(0.001, 0.001)
  
  # Train predictions
  for(i in 1:n){
    yf[i] ~ dnorm(mu[i], tau.y) 
  }
  
  # Test predictions
  for(i in 1:n_test){
    yf_test[i] ~ dnorm(mu_test[i], tau.y) 
    mu_test[i] <- alpha[zip_code_test[i]] + 
                  beta_1[zip_code_test[i]]*x_test[i] + 
                  beta_2[zip_code_test[i]]*building_class_test[i] + 
                  beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
  }

}"

write_file(string_mod_13,
           path = "model_13.model")

inits_13 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
    mu.a = rep(0, nyc_sales_list$n_borough),
    mu.b1 = rep(0, nyc_sales_list$n_borough),
    mu.b2 = rep(0, nyc_sales_list$n_borough),
    mu.b3 = rep(0, nyc_sales_list$n_borough),
    tau.a = rep(1, nyc_sales_list$n_borough),
    tau.b1 = rep(1, nyc_sales_list$n_borough),
    tau.b2 = rep(1, nyc_sales_list$n_borough),
    tau.b3 = rep(1, nyc_sales_list$n_borough),
    mu.0.a = 0,
    lambda.a =1,
    mu.0.b1 = 0,
    lambda.b1 =1,
    mu.0.b2 = 0,
    lambda.b2 =1,
    mu.0.b3 = 0,
    lambda.b3 =1,
    tau.a.0 = 1,
    tau.0.b1 = 1,
    tau.0.b2 = 1,
    tau.0.b3 = 1
  )
}

parameters_13 <- c("mu.a", 
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
                   "mu.0.a",
                   "lambda.a",
                   "mu.0.b1",
                   "lambda.b1",
                   "mu.0.b2",
                   "lambda.b2",
                   "mu.0.b3 ",
                   "lambda.b3 ",
                   "tau.a.0 ",
                   "tau.0.b1",
                   "tau.0.b2",
                   "tau.0.b3",
                   "yf",
                   "yf_test"
                   
)

# sim_13 <- jags(nyc_sales_list,
#                inits_13,
#                parameters_13,
#                model.file = "model_13.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_13, "../out/models/model_13.rds")


if("summary_mod_13.rds" %in% file_list){
  summary_mod_13 <- read_rds("../out/models/summary_mod_13.rds")
} else {
  summary_mod_13 <- read_rds("../out/models/model_13.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_13, "../out/models/summary_mod_13.rds")
  gc()
}

summary_mod_13 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_13 <- summary_mod_13 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_13 <- summary_mod_13 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_13 <- sqrt(mean(preds_13$res^2)))
(rmse_train_log_13 <- sqrt(mean((preds_13$mean - log(preds_13$obs))^2)))
(rmse_test_13 <- sqrt(mean(preds_test_13$res^2)))
(rmse_test_log_13 <- sqrt(mean((preds_test_13$mean - log(preds_test_13$obs))^2)))

(preds_13 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_13)

(preds_13 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_13)

(preds_test_13 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_13)

(preds_test_13 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_13)



preds_13 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_13 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)







#### Modelo 14: Igual que el 13 pero con neighborhoods en lugar de boroughs


string_mod_14 <- "model {
for(i in 1:n) {
y[i] ~ dnorm(mu[i], tau.y) 
mu[i] <-  alpha[zip_code[i]] + 
beta_1[zip_code[i]]*x[i] + 
beta_2[zip_code[i]]*building_class[i] + 
beta_3[zip_code[i]]*building_class[i]*x[i]
}
tau.y ~ dgamma(0.001, 0.001)

for(j in 1:n_zip){
alpha[j] ~ dnorm(mu.a[neighborhood[j]], tau.a[neighborhood[j]])
beta_1[j] ~ dnorm(mu.b1[neighborhood[j]], tau.b1[neighborhood[j]])
beta_2[j] ~ dnorm(mu.b2[neighborhood[j]], tau.b2[neighborhood[j]])
beta_3[j] ~ dnorm(mu.b3[neighborhood[j]], tau.b3[neighborhood[j]])
}

for(z in 1:n_neighborhood){
mu.a[z] ~ dnorm(mu.0.a, tau.a.0)
mu.b1[z] ~ dnorm(mu.0.b1, tau.0.b1)
mu.b2[z] ~ dnorm(mu.0.b2, tau.0.b2)
mu.b3[z] ~ dnorm(mu.0.b3, tau.0.b3)
tau.a[z] ~ dexp(lambda.a)
tau.b1[z] ~dexp(lambda.b1)
tau.b2[z] ~dexp(lambda.b2)
tau.b3[z] ~ dexp(lambda.b3)
}

mu.0.a ~ dnorm(0, 0.0001)
lambda.a ~ dgamma(0.001, 0.001)
mu.0.b1 ~ dnorm(0, 0.0001)
lambda.b1 ~ dgamma(0.001, 0.001)
mu.0.b2 ~ dnorm(0, 0.0001)
lambda.b2 ~ dgamma(0.001, 0.001)
mu.0.b3 ~ dnorm(0, 0.0001)
lambda.b3 ~ dgamma(0.001, 0.001)
tau.a.0 ~ dgamma(0.001, 0.001)
tau.0.b1 ~ dgamma(0.001, 0.001)
tau.0.b2 ~ dgamma(0.001, 0.001)
tau.0.b3 ~ dgamma(0.001, 0.001)

# Train predictions
for(i in 1:n){
yf[i] ~ dnorm(mu[i], tau.y) 
}

# Test predictions
for(i in 1:n_test){
yf_test[i] ~ dnorm(mu_test[i], tau.y) 
mu_test[i] <- alpha[zip_code_test[i]] + 
beta_1[zip_code_test[i]]*x_test[i] + 
beta_2[zip_code_test[i]]*building_class_test[i] + 
beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
}

}"

write_file(string_mod_14,
           path = "model_14.model")

inits_14 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
    mu.a = rep(0, nyc_sales_list$n_neighborhood),
    mu.b1 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b2 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b3 = rep(0, nyc_sales_list$n_neighborhood),
    tau.a = rep(1, nyc_sales_list$n_neighborhood),
    tau.b1 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b2 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b3 = rep(1, nyc_sales_list$n_neighborhood),
    mu.0.a = 0,
    lambda.a =1,
    mu.0.b1 = 0,
    lambda.b1 =1,
    mu.0.b2 = 0,
    lambda.b2 =1,
    mu.0.b3 = 0,
    lambda.b3 =1,
    tau.a.0 = 1,
    tau.0.b1 = 1,
    tau.0.b2 = 1,
    tau.0.b3 = 1,
    tau.y = 1
  )
}

parameters_14 <- c("mu.a", 
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
                   "mu.0.a",
                   "lambda.a",
                   "mu.0.b1",
                   "lambda.b1",
                   "mu.0.b2",
                   "lambda.b2",
                   "mu.0.b3 ",
                   "lambda.b3 ",
                   "tau.a.0 ",
                   "tau.0.b1",
                   "tau.0.b2",
                   "tau.0.b3",
                   "yf",
                   "yf_test"
                   
)

# sim_14 <- jags(nyc_sales_list,
#                inits_14,
#                parameters_14,
#                model.file = "model_14.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_14, "../out/models/model_14.rds")


if("summary_mod_14.rds" %in% file_list){
  summary_mod_14 <- read_rds("../out/models/summary_mod_14.rds")
} else {
  summary_mod_14 <- read_rds("../out/models/model_14.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_14, "../out/models/summary_mod_14.rds")
  gc()
}

summary_mod_14 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_14 <- summary_mod_14 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_14 <- summary_mod_14 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_14 <- sqrt(mean(preds_14$res^2)))
(rmse_train_log_14 <- sqrt(mean((preds_14$mean - log(preds_14$obs))^2)))
(rmse_test_14 <- sqrt(mean(preds_test_14$res^2)))
(rmse_test_log_14 <- sqrt(mean((preds_test_14$mean - log(preds_test_14$obs))^2)))

(preds_14 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_14)

(preds_14 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_14)

(preds_test_14 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_14)

(preds_test_14 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_14)



preds_14 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_14 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)








#### Modelo 15: Igual que el 14 pero sin covariables


string_mod_15 <- "model {
for(i in 1:n) {
  y[i] ~ dnorm(mu[i], tau.y) 
  mu[i] <-  alpha[zip_code[i]] + 
            beta_1[zip_code[i]]*x[i]
}
tau.y ~ dgamma(0.001, 0.001)

for(j in 1:n_zip){
  alpha[j] ~ dnorm(mu.a[neighborhood[j]], tau.a[neighborhood[j]])
  beta_1[j] ~ dnorm(mu.b1[neighborhood[j]], tau.b1[neighborhood[j]])
}

for(z in 1:n_neighborhood){
  mu.a[z] ~ dnorm(mu.0.a, tau.a.0)
  mu.b1[z] ~ dnorm(mu.0.b1, tau.0.b1)
  tau.a[z] ~ dexp(lambda.a)
  tau.b1[z] ~dexp(lambda.b1)
}

mu.0.a ~ dnorm(0, 0.0001)
lambda.a ~ dgamma(0.001, 0.001)
mu.0.b1 ~ dnorm(0, 0.0001)
lambda.b1 ~ dgamma(0.001, 0.001)
tau.a.0 ~ dgamma(0.001, 0.001)
tau.0.b1 ~ dgamma(0.001, 0.001)


# Train predictions
for(i in 1:n){
  yf[i] ~ dnorm(mu[i], tau.y) 
}

# Test predictions
for(i in 1:n_test){
  yf_test[i] ~ dnorm(mu_test[i], tau.y) 
  mu_test[i] <- alpha[zip_code_test[i]] + 
                beta_1[zip_code_test[i]]*x_test[i]
}

}"

write_file(string_mod_15,
           path = "model_15.model")

inits_15 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    mu.a = rep(0, nyc_sales_list$n_neighborhood),
    mu.b1 = rep(0, nyc_sales_list$n_neighborhood),
    tau.a = rep(1, nyc_sales_list$n_neighborhood),
    tau.b1 = rep(1, nyc_sales_list$n_neighborhood),
    mu.0.a = 0,
    lambda.a =1,
    mu.0.b1 = 0,
    lambda.b1 =1,
    tau.a.0 = 1,
    tau.0.b1 = 1,
    tau.y = 1
  )
}

parameters_15 <- c("mu.a", 
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
                   "mu.0.a",
                   "lambda.a",
                   "mu.0.b1",
                   "lambda.b1",
                   "mu.0.b2",
                   "lambda.b2",
                   "mu.0.b3 ",
                   "lambda.b3 ",
                   "tau.a.0 ",
                   "tau.0.b1",
                   "tau.0.b2",
                   "tau.0.b3",
                   "yf",
                   "yf_test"
                   
)

# sim_15 <- jags(nyc_sales_list,
#                inits_15,
#                parameters_15,
#                model.file = "model_15.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_15, "../out/models/model_15.rds")


if("summary_mod_15.rds" %in% file_list){
  summary_mod_15 <- read_rds("../out/models/summary_mod_15.rds")
} else {
  summary_mod_15 <- read_rds("../out/models/model_15.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_15, "../out/models/summary_mod_15.rds")
  gc()
}

summary_mod_15 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_15 <- summary_mod_15 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_15 <- summary_mod_15 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_15 <- sqrt(mean(preds_15$res^2)))
(rmse_train_log_15 <- sqrt(mean((preds_15$mean - log(preds_15$obs))^2)))
(rmse_test_15 <- sqrt(mean(preds_test_15$res^2)))
(rmse_test_log_15 <- sqrt(mean((preds_test_15$mean - log(preds_test_15$obs))^2)))

(preds_15 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_15)

(preds_15 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_15)

(preds_test_15 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_15)

(preds_test_15 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_15)



preds_15 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_15 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)








#### Modelo 16: Igual que el 14 pero con varianzas variantes por zip code


string_mod_16 <- "model {
for(i in 1:n) {
  y[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
  mu[i] <-  alpha[zip_code[i]] + 
            beta_1[zip_code[i]]*x[i] + 
            beta_2[zip_code[i]]*building_class[i] + 
            beta_3[zip_code[i]]*building_class[i]*x[i]
}

for(j in 1:n_zip){
  alpha[j] ~ dnorm(mu.a[neighborhood[j]], tau.a[neighborhood[j]])
  beta_1[j] ~ dnorm(mu.b1[neighborhood[j]], tau.b1[neighborhood[j]])
  beta_2[j] ~ dnorm(mu.b2[neighborhood[j]], tau.b2[neighborhood[j]])
  beta_3[j] ~ dnorm(mu.b3[neighborhood[j]], tau.b3[neighborhood[j]])
  tau.y[j] ~ dgamma(tau.y.a[neighborhood[j]], tau.y.b[neighborhood[j]])
}

for(z in 1:n_neighborhood){
  mu.a[z] ~ dnorm(mu.0.a, tau.a.0)
  mu.b1[z] ~ dnorm(mu.0.b1, tau.0.b1)
  mu.b2[z] ~ dnorm(mu.0.b2, tau.0.b2)
  mu.b3[z] ~ dnorm(mu.0.b3, tau.0.b3)
  tau.a[z] ~ dexp(lambda.a)
  tau.b1[z] ~dexp(lambda.b1)
  tau.b2[z] ~dexp(lambda.b2)
  tau.b3[z] ~ dexp(lambda.b3)
  tau.y.a[z] ~ dexp(lambda.y.a)
  tau.y.b[z] ~ dexp(lambda.y.b)
}

mu.0.a ~ dnorm(0, 0.0001)
lambda.a ~ dgamma(0.001, 0.001)
mu.0.b1 ~ dnorm(0, 0.0001)
lambda.b1 ~ dgamma(0.001, 0.001)
mu.0.b2 ~ dnorm(0, 0.0001)
lambda.b2 ~ dgamma(0.001, 0.001)
mu.0.b3 ~ dnorm(0, 0.0001)
lambda.b3 ~ dgamma(0.001, 0.001)
lambda.y.a ~ dgamma(0.001, 0.001)
lambda.y.b ~ dgamma(0.001, 0.001)
tau.a.0 ~ dgamma(0.001, 0.001)
tau.0.b1 ~ dgamma(0.001, 0.001)
tau.0.b2 ~ dgamma(0.001, 0.001)
tau.0.b3 ~ dgamma(0.001, 0.001)

# Train predictions
  for(i in 1:n){
  yf[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
}

# Test predictions
for(i in 1:n_test){
  yf_test[i] ~ dnorm(mu_test[i], tau.y[zip_code_test[i]]) 
  mu_test[i] <- alpha[zip_code_test[i]] + 
                beta_1[zip_code_test[i]]*x_test[i] + 
                beta_2[zip_code_test[i]]*building_class_test[i] + 
                beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
}

}"

write_file(string_mod_16,
           path = "model_16.model")

inits_16 <- function(){
  list(
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
    mu.a = rep(0, nyc_sales_list$n_neighborhood),
    mu.b1 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b2 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b3 = rep(0, nyc_sales_list$n_neighborhood),
    tau.a = rep(1, nyc_sales_list$n_neighborhood),
    tau.b1 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b2 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b3 = rep(1, nyc_sales_list$n_neighborhood),
    mu.0.a = 0,
    lambda.a =1,
    mu.0.b1 = 0,
    lambda.b1 =1,
    mu.0.b2 = 0,
    lambda.b2 =1,
    mu.0.b3 = 0,
    lambda.b3 =1,
    tau.a.0 = 1,
    tau.0.b1 = 1,
    tau.0.b2 = 1,
    tau.0.b3 = 1,
    tau.y = rep(1, nyc_sales_list$n_zip),
    lambda.y.a = 1,
    lambda.y.b = 1
  )
}

parameters_16 <- c("mu.a", 
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
                   "mu.0.a",
                   "lambda.a",
                   "mu.0.b1",
                   "lambda.b1",
                   "mu.0.b2",
                   "lambda.b2",
                   "mu.0.b3 ",
                   "lambda.b3 ",
                   "tau.a.0 ",
                   "tau.0.b1",
                   "tau.0.b2",
                   "tau.0.b3",
                   "yf",
                   "yf_test",
                   "lambda.y.a",
                   "lambda.y.b"
                   
)

# sim_16 <- jags(nyc_sales_list,
#                inits_16,
#                parameters_16,
#                model.file = "model_16.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_16, "../out/models/model_16.rds")
# summary_mod_16 <- sim_16$BUGSoutput$summary  
# saveRDS(summary_mod_16, "../out/models/summary_mod_16.rds")

if("summary_mod_16.rds" %in% file_list){
  summary_mod_16 <- read_rds("../out/models/summary_mod_16.rds")
} else {
  summary_mod_16 <- read_rds("../out/models/model_16.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_16, "../out/models/summary_mod_16.rds")
  gc()
}

summary_mod_16 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_16 <- summary_mod_16 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_16 <- summary_mod_16 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_16 <- sqrt(mean(preds_16$res^2)))
(rmse_train_log_16 <- sqrt(mean((preds_16$mean - log(preds_16$obs))^2)))
(rmse_test_16 <- sqrt(mean(preds_test_16$res^2)))
(rmse_test_log_16 <- sqrt(mean((preds_test_16$mean - log(preds_test_16$obs))^2)))

(preds_16 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_16)

(preds_16 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_16)

(preds_test_16 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_16)

(preds_test_16 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_16)



preds_16 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_16 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)







#### Modelo 21: Tres niveles con varianza constante para y


string_mod_21 <- "model {
for(i in 1:n) {
  y[i] ~ dnorm(mu[i], tau.y) 
  mu[i] <-  alpha[zip_code[i]] + 
            beta_1[zip_code[i]]*x[i] + 
            beta_2[zip_code[i]]*building_class[i] + 
            beta_3[zip_code[i]]*building_class[i]*x[i]
}
tau.y ~ dgamma(0.001, 0.001)

for(j in 1:n_zip){
  alpha[j] ~ dnorm(mu.a[neighborhood[j]], tau.a[neighborhood[j]])
  beta_1[j] ~ dnorm(mu.b1[neighborhood[j]], tau.b1[neighborhood[j]])
  beta_2[j] ~ dnorm(mu.b2[neighborhood[j]], tau.b2[neighborhood[j]])
  beta_3[j] ~ dnorm(mu.b3[neighborhood[j]], tau.b3[neighborhood[j]])
}

for(j in 1:n_neighborhood){
  mu.a[j] ~ dnorm(mu.a.1[borough[j]], tau.a.1[borough[j]])
  tau.a[j] ~ dexp(lambda.a.1[borough[j]])
  mu.b1[j] ~ dnorm(mu.b1.1[borough[j]], tau.b1.1[borough[j]])
  tau.b1[j] ~ dexp(lambda.b1.1[borough[j]])
  mu.b2[j] ~ dnorm(mu.b2.1[borough[j]], tau.b2.1[borough[j]])
  tau.b2[j] ~ dexp(lambda.b2.1[borough[j]])
  mu.b3[j] ~ dnorm(mu.b3.1[borough[j]], tau.b3.1[borough[j]])
  tau.b3[j] ~ dexp(lambda.b3.1[borough[j]])
}

for(j in 1:n_borough){
  mu.a.1[j] ~ dnorm(mu.a.1.0, tau.a.1.0)
  tau.a.1[j] ~ dexp(lambda.a.tau.0)
  lambda.a.1[j] ~ dexp(lambda.a.0)

  mu.b1.1[j] ~ dnorm(mu.b1.1.0, tau.b1.1.0)
  tau.b1.1[j] ~ dexp(lambda.b1.tau.0)
  lambda.b1.1[j] ~ dexp(lambda.b1.0)

  mu.b2.1[j] ~ dnorm(mu.b2.1.0, tau.b2.1.0)
  tau.b2.1[j] ~ dexp(lambda.b2.tau.0)
  lambda.b2.1[j] ~ dexp(lambda.b2.0)

  mu.b3.1[j] ~ dnorm(mu.b3.1.0, tau.b3.1.0)
  tau.b3.1[j] ~ dexp(lambda.b3.tau.0)
  lambda.b3.1[j] ~ dexp(lambda.b3.0)
}

mu.a.1.0 ~ dnorm(0, 0.0001)
tau.a.1.0 ~ dgamma(0.01, 0.01)
lambda.a.tau.0 ~ dgamma(0.01, 0.01)
lambda.a.0 ~ dgamma(0.01, 0.01)
mu.b1.1.0 ~ dnorm(0, 0.0001)
tau.b1.1.0 ~ dgamma(0.01, 0.01)
lambda.b1.tau.0 ~ dgamma(0.01, 0.01)
lambda.b1.0 ~ dgamma(0.01, 0.01)
mu.b2.1.0 ~ dnorm(0, 0.0001)
tau.b2.1.0 ~ dgamma(0.01, 0.01)
lambda.b2.tau.0 ~ dgamma(0.01, 0.01)
lambda.b2.0 ~ dgamma(0.01, 0.01)
mu.b3.1.0 ~ dnorm(0, 0.0001)
tau.b3.1.0 ~ dgamma(0.01, 0.01)
lambda.b3.tau.0 ~ dgamma(0.01, 0.01)
lambda.b3.0 ~ dgamma(0.01, 0.01)

# Train predictions
for(i in 1:n){
  yf[i] ~ dnorm(mu[i], tau.y) 
}

# Test predictions
for(i in 1:n_test){
  yf_test[i] ~ dnorm(mu_test[i], tau.y) 
  mu_test[i] <- alpha[zip_code_test[i]] + 
                beta_1[zip_code_test[i]]*x_test[i] + 
                beta_2[zip_code_test[i]]*building_class_test[i] + 
                beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
}

}"

write_file(string_mod_21,
           path = "model_21.model")

inits_21 <- function(){
  list(
    tau.y = 1,
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
    mu.a = rep(0, nyc_sales_list$n_neighborhood),
    mu.b1 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b2 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b3 = rep(0, nyc_sales_list$n_neighborhood),
    tau.a = rep(1, nyc_sales_list$n_neighborhood),
    tau.b1 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b2 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b3 = rep(1, nyc_sales_list$n_neighborhood),
    
    mu.a.1 = rep(0, nyc_sales_list$n_borough),
    tau.a.1 = rep(1, nyc_sales_list$n_borough),
    lambda.a.1 = rep(1, nyc_sales_list$n_borough),
    mu.b1.1 = rep(0, nyc_sales_list$n_borough),
    tau.b1.1 = rep(1, nyc_sales_list$n_borough),
    lambda.b1.1 = rep(1, nyc_sales_list$n_borough),
    mu.b2.1 = rep(0, nyc_sales_list$n_borough),
    tau.b2.1 = rep(1, nyc_sales_list$n_borough),
    lambda.b2.1 = rep(1, nyc_sales_list$n_borough),
    mu.b3.1 = rep(0, nyc_sales_list$n_borough),
    tau.b3.1 = rep(1, nyc_sales_list$n_borough),
    lambda.b3.1 = rep(1, nyc_sales_list$n_borough),
    
    mu.a.1.0 = 0,
    tau.a.1.0 = 1,
    lambda.a.tau.0 = 1,
    lambda.a.0 = 1,
    mu.b1.1.0 = 0,
    tau.b1.1.0 = 1,
    lambda.b1.tau.0 = 1,
    lambda.b1.0 = 1,
    mu.b2.1.0 = 0,
    tau.b2.1.0 = 1,
    lambda.b2.tau.0 = 1,
    lambda.b2.0 = 1,
    mu.b3.1.0 = 0,
    tau.b3.1.0 = 1,
    lambda.b3.tau.0 = 1,
    lambda.b3.0 = 1
  )
}

parameters_21 <- c("tau.y",
                   "alpha",
                   "beta_1",
                   "beta_2",
                   "beta_3",
                   "mu.a",
                   "mu.b1",
                   "mu.b2",
                   "mu.b3",
                   "tau.a",
                   "tau.b1",
                   "tau.b2",
                   "tau.b3",
                   
                   "mu.a.1",
                   "tau.a.1",
                   "lambda.a.1",
                   "mu.b1.1",
                   "tau.b1.1",
                   "lambda.b1.1",
                   "mu.b2.1",
                   "tau.b2.1",
                   "lambda.b2.1",
                   "mu.b3.1",
                   "tau.b3.1",
                   "lambda.b3.1",
                   
                   "mu.a.1.0",
                   "tau.a.1.0",
                   "lambda.a.tau.0",
                   "lambda.a.0",
                   "mu.b1.1.0",
                   "tau.b1.1.0",
                   "lambda.b1.tau.0",
                   "lambda.b1.0",
                   "mu.b2.1.0",
                   "tau.b2.1.0",
                   "lambda.b2.tau.0",
                   "lambda.b2.0",
                   "mu.b3.1.0",
                   "tau.b3.1.0",
                   "lambda.b3.tau.0",
                   "lambda.b3.0",
                   "yf",
                   "yf_test"
                   
)

# sim_21 <- jags(nyc_sales_list,
#                inits_21,
#                parameters_21,
#                model.file = "model_21.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_21, "../out/models/model_21.rds")
# summary_mod_21 <- sim_21$BUGSoutput$summary  
# saveRDS(summary_mod_21, "../out/models/summary_mod_21.rds")


if("summary_mod_21.rds" %in% file_list){
  summary_mod_21 <- read_rds("../out/models/summary_mod_21.rds")
} else {
  summary_mod_21 <- read_rds("../out/models/model_21.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_21, "../out/models/summary_mod_21.rds")
  gc()
}

summary_mod_21 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_21 <- summary_mod_21 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_21 <- summary_mod_21 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_21 <- sqrt(mean(preds_21$res^2)))
(rmse_train_log_21 <- sqrt(mean((preds_21$mean - log(preds_21$obs))^2)))
(rmse_test_21 <- sqrt(mean(preds_test_21$res^2)))
(rmse_test_log_21 <- sqrt(mean((preds_test_21$mean - log(preds_test_21$obs))^2)))

(preds_21 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_21)

(preds_21 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_21)

(preds_test_21 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_21)

(preds_test_21 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_21)



preds_21 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_21 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)




#### Modelo 22: Igual que el 21 pero con varianzas distintas para las y


string_mod_22 <- "model {
for(i in 1:n) {
  y[i] ~ dnorm(mu[i], tau.y[zip_code[i]]) 
  mu[i] <-  alpha[zip_code[i]] + 
            beta_1[zip_code[i]]*x[i] + 
            beta_2[zip_code[i]]*building_class[i] + 
            beta_3[zip_code[i]]*building_class[i]*x[i]
}

for(j in 1:n_zip){
  alpha[j] ~ dnorm(mu.a[neighborhood[j]], tau.a[neighborhood[j]])
  beta_1[j] ~ dnorm(mu.b1[neighborhood[j]], tau.b1[neighborhood[j]])
  beta_2[j] ~ dnorm(mu.b2[neighborhood[j]], tau.b2[neighborhood[j]])
  beta_3[j] ~ dnorm(mu.b3[neighborhood[j]], tau.b3[neighborhood[j]])
  tau.y[j] ~ dgamma(alpha.y[borough[j]], beta.y[borough[j]])
}

for(j in 1:n_neighborhood){
  mu.a[j] ~ dnorm(mu.a.1[borough[j]], tau.a.1[borough[j]])
  tau.a[j] ~ dexp(lambda.a.1[borough[j]])
  mu.b1[j] ~ dnorm(mu.b1.1[borough[j]], tau.b1.1[borough[j]])
  tau.b1[j] ~ dexp(lambda.b1.1[borough[j]])
  mu.b2[j] ~ dnorm(mu.b2.1[borough[j]], tau.b2.1[borough[j]])
  tau.b2[j] ~ dexp(lambda.b2.1[borough[j]])
  mu.b3[j] ~ dnorm(mu.b3.1[borough[j]], tau.b3.1[borough[j]])
  tau.b3[j] ~ dexp(lambda.b3.1[borough[j]])
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
  
  mu.b2.1[j] ~ dnorm(mu.b2.1.0, tau.b2.1.0)
  tau.b2.1[j] ~ dexp(lambda.b2.tau.0)
  lambda.b2.1[j] ~ dexp(lambda.b2.0)
  
  mu.b3.1[j] ~ dnorm(mu.b3.1.0, tau.b3.1.0)
  tau.b3.1[j] ~ dexp(lambda.b3.tau.0)
  lambda.b3.1[j] ~ dexp(lambda.b3.0)

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
mu.b2.1.0 ~ dnorm(0, 0.0001)
tau.b2.1.0 ~ dgamma(0.01, 0.01)
lambda.b2.tau.0 ~ dgamma(0.01, 0.01)
lambda.b2.0 ~ dgamma(0.01, 0.01)
mu.b3.1.0 ~ dnorm(0, 0.0001)
tau.b3.1.0 ~ dgamma(0.01, 0.01)
lambda.b3.tau.0 ~ dgamma(0.01, 0.01)
lambda.b3.0 ~ dgamma(0.01, 0.01)
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
                beta_1[zip_code_test[i]]*x_test[i] + 
                beta_2[zip_code_test[i]]*building_class_test[i] + 
                beta_3[zip_code_test[i]]*building_class_test[i]*x_test[i]
}

}"

write_file(string_mod_22,
           path = "model_22.model")

inits_22 <- function(){
  list(
    tau.y = rep(1, nyc_sales_list$n_zip),
    alpha = rep(0, nyc_sales_list$n_zip), 
    beta_1 = rep(0, nyc_sales_list$n_zip), 
    beta_2 = rep(0, nyc_sales_list$n_zip), 
    beta_3 = rep(0, nyc_sales_list$n_zip), 
    mu.a = rep(0, nyc_sales_list$n_neighborhood),
    mu.b1 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b2 = rep(0, nyc_sales_list$n_neighborhood),
    mu.b3 = rep(0, nyc_sales_list$n_neighborhood),
    tau.a = rep(1, nyc_sales_list$n_neighborhood),
    tau.b1 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b2 = rep(1, nyc_sales_list$n_neighborhood),
    tau.b3 = rep(1, nyc_sales_list$n_neighborhood),
    alpha.y = rep(1, nyc_sales_list$n_neighborhood),
    beta.y = rep(1, nyc_sales_list$n_neighborhood),
    
    mu.a.1 = rep(0, nyc_sales_list$n_borough),
    tau.a.1 = rep(1, nyc_sales_list$n_borough),
    lambda.a.1 = rep(1, nyc_sales_list$n_borough),
    mu.b1.1 = rep(0, nyc_sales_list$n_borough),
    tau.b1.1 = rep(1, nyc_sales_list$n_borough),
    lambda.b1.1 = rep(1, nyc_sales_list$n_borough),
    mu.b2.1 = rep(0, nyc_sales_list$n_borough),
    tau.b2.1 = rep(1, nyc_sales_list$n_borough),
    lambda.b2.1 = rep(1, nyc_sales_list$n_borough),
    mu.b3.1 = rep(0, nyc_sales_list$n_borough),
    tau.b3.1 = rep(1, nyc_sales_list$n_borough),
    lambda.b3.1 = rep(1, nyc_sales_list$n_borough),
    lambda.alpha.y = rep(1, nyc_sales_list$n_borough),
    lambda.beta.y = rep(1, nyc_sales_list$n_borough),
    
    mu.a.1.0 = 0,
    tau.a.1.0 = 1,
    lambda.a.tau.0 = 1,
    lambda.a.0 = 1,
    mu.b1.1.0 = 0,
    tau.b1.1.0 = 1,
    lambda.b1.tau.0 = 1,
    lambda.b1.0 = 1,
    mu.b2.1.0 = 0,
    tau.b2.1.0 = 1,
    lambda.b2.tau.0 = 1,
    lambda.b2.0 = 1,
    mu.b3.1.0 = 0,
    tau.b3.1.0 = 1,
    lambda.b3.tau.0 = 1,
    lambda.b3.0 = 1,
    lambda.lambda.alpha.y = 1,
    lambda.lambda.beta.y = 1
  )
}

parameters_22 <- c("tau.y",
                   "alpha",
                   "beta_1",
                   "beta_2",
                   "beta_3",
                   "mu.a",
                   "mu.b1",
                   "mu.b2",
                   "mu.b3",
                   "tau.a",
                   "tau.b1",
                   "tau.b2",
                   "tau.b3",
                   "alpha.y",
                   "beta.y",
                   
                   "mu.a.1",
                   "tau.a.1",
                   "lambda.a.1",
                   "mu.b1.1",
                   "tau.b1.1",
                   "lambda.b1.1",
                   "mu.b2.1",
                   "tau.b2.1",
                   "lambda.b2.1",
                   "mu.b3.1",
                   "tau.b3.1",
                   "lambda.b3.1",
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
                   "mu.b2.1.0",
                   "tau.b2.1.0",
                   "lambda.b2.tau.0",
                   "lambda.b2.0",
                   "mu.b3.1.0",
                   "tau.b3.1.0",
                   "lambda.b3.tau.0",
                   "lambda.b3.0",
                   "lambda.lambda.alpha.y",
                   "lambda.lambda.beta.y",
                   
                   "yf",
                   "yf_test"
                   
)

# sim_22 <- jags(nyc_sales_list,
#                inits_22,
#                parameters_22,
#                model.file = "model_22.model",
#                n.iter = 10000,
#                n.chains = 1,
#                n.thin = 2,
#                n.burnin = 2000)
# 
# saveRDS(sim_22, "../out/models/model_22.rds")
# summary_mod_22 <- sim_22$BUGSoutput$summary  
# saveRDS(summary_mod_22, "../out/models/summary_mod_22.rds")


if("summary_mod_22.rds" %in% file_list){
  summary_mod_22 <- read_rds("../out/models/summary_mod_22.rds")
} else {
  summary_mod_22 <- read_rds("../out/models/model_22.rds")$BUGSoutput$summary  
  saveRDS(summary_mod_22, "../out/models/summary_mod_22.rds")
  gc()
}

summary_mod_22 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  as_tibble()

preds_22 <- summary_mod_22 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_22 <- summary_mod_22 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))


(rmse_train_22 <- sqrt(mean(preds_22$res^2)))
(rmse_train_log_22 <- sqrt(mean((preds_22$mean - log(preds_22$obs))^2)))
(rmse_test_22 <- sqrt(mean(preds_test_22$res^2)))
(rmse_test_log_22 <- sqrt(mean((preds_test_22$mean - log(preds_test_22$obs))^2)))

(preds_22 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_22)

(preds_22 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_22)

(preds_test_22 %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_22)

(preds_test_22 %>% 
    mutate(in_interval = (log( obs) >= X25. & log( obs) <= X75.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_22)



preds_22 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_22 %>% 
  mutate(BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean, color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1)


preds_test_22 %>%
  ggplot(aes(log(obs), mean)) +
  geom_point(alpha = 0.4, size = 0.4) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
  geom_abline(slope = 1)

preds_22 %>% 
  mutate(Neighborhood = paste(nyc_train$Borough, nyc_train$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

preds_22 %>% 
  mutate(Neighborhood = paste(nyc_train$Borough, nyc_train$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
  ggplot(aes(x = log(obs), y = X50., color = BUILDING_CLASS_CATEGORY)) +
  geom_point(alpha = 0.6) + geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

preds_test_22 %>% 
  mutate(Neighborhood = paste(nyc_test$Borough, nyc_test$Neighborhood),
         BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
  ggplot() +
  geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)




rmse_train_1 
rmse_test_1 

rmse_train_2 
rmse_test_2 

rmse_train_6 
rmse_test_6 

rmse_train_7 
rmse_test_7 

rmse_train_8 
rmse_test_8 

rmse_train_9 
rmse_test_9 

rmse_train_11 
rmse_test_11 

rmse_train_12 
rmse_test_12 

rmse_train_13 
rmse_test_13 

rmse_train_14 
rmse_test_14 

rmse_train_15 
rmse_test_15 

rmse_train_16 
rmse_test_16 

rmse_train_21 
rmse_test_21

rmse_train_22 
rmse_test_22




# ### frecuentistas
# 
# mod2 <- lm(log(SALE_PRICE) ~ log(GROSS_SQUARE_FEET) + Borough + BUILDING_CLASS_CATEGORY*log(GROSS_SQUARE_FEET), 
#            data = nyc_train)
# 
# summary(mod2)
# 
# tibble(
#   yhat = predict(mod2, nyc_train),
#   obs = log(nyc_train$SALE_PRICE)) %>% 
#   ggplot() +
#   geom_point(aes(obs, yhat)) +
#   geom_abline(slope = 1)
# 
# tibble(
#   log_res = predict(mod2, nyc_train) - log(nyc_train$SALE_PRICE),
#   BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   sample_n(nrow(.)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, log_res, color = BUILDING_CLASS_CATEGORY), alpha = 0.7)
# 
# 
# tibble(
#   res = exp(predict(mod2, nyc_train)) - nyc_train$SALE_PRICE,
#   BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   sample_n(nrow(.)) %>% 
#   mutate(ix = 1:nrow(.)) %>% 
#   ggplot() +
#   geom_point(aes(ix, res, color = BUILDING_CLASS_CATEGORY), alpha = 0.7)
# 
# tibble(
#   estimate = exp(predict(mod2, nyc_train)),
#   price = nyc_train$SALE_PRICE,
#   zip_code = nyc_train$zip_code,
#   GROSS_SQUARE_FEET = nyc_train$GROSS_SQUARE_FEET,
#   res = exp(predict(mod2, nyc_train)) - nyc_train$SALE_PRICE,
#   Address = nyc_train$ADDRESS,
#   BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   sample_n(nrow(.)) %>% 
#   mutate(ix = 1:nrow(.)) %>% filter(res > 1.5e06 | res < -1.5e06) %>% 
#   View
# 
# 
