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


rmse_train_1 <- mean(preds_1$res^2)
rmse_train_log_1 <- mean((preds_1$mean - log(preds_1$obs))^2)
rmse_test_1 <- mean(preds_test_1$res^2)
rmse_test_log_1 <- mean((preds_test_1$mean - log(preds_test_1$obs))^2)

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
  summary_mod_2 <- read_rds("../out/models/model_01.rds")$BUGSoutput$summary  
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


rmse_train_2 <- mean(preds_2$res^2)
rmse_train_log_2 <- mean((preds_2$mean - log(preds_2$obs))^2)
rmse_test_2 <- mean(preds_test_2$res^2)
rmse_test_log_2 <- mean((preds_test_2$mean - log(preds_test_2$obs))^2)

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
  summary_mod_3 <- read_rds("../out/models/model_01.rds")$BUGSoutput$summary  
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
#     mu[i] <- alpha[neighborhood[i]] + beta_1*x[i] + beta_2*building_class[i] + beta_3*building_class[i]*x[i]
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
#     mu[i] <- alpha[zip_code[i]] + beta_1*x[i] + beta_2*building_class[i] + beta_3*building_class[i]*x[i]
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
# rmse_train_5 <- mean(preds_5$res^2)
# rmse_train_log_5 <- mean((preds_5$mean - log(preds_5$obs))^2)
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




## Modelo 6: Jerárquico con zip codes, tipo de edificio como covariable y pendientes e interceptos distintos

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
  summary_mod_6 <- read_rds("../out/models/model_01.rds")$BUGSoutput$summary  
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




rmse_train_6 <- mean(preds_6$res^2)
rmse_train_log_6 <- mean((preds_6$mean - log(preds_6$obs))^2)
rmse_test_6 <- mean(preds_test_6$res^2)
rmse_test_log_6 <- mean((preds_test_6$mean - log(preds_test_6$obs))^2)

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






## Modelo 7: Jerárquico con vecindarios, tipo de edificio como covariable y pendientes e interceptos distintos

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
  summary_mod_7 <- read_rds("../out/models/model_01.rds")$BUGSoutput$summary  
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


rmse_train_7 <- mean(preds_7$res^2)
rmse_train_log_7 <- mean((preds_7$mean - log(preds_7$obs))^2)
rmse_test_7 <- mean(preds_test_7$res^2)
rmse_test_log_7 <- mean((preds_test_7$mean - log(preds_test_7$obs))^2)

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
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
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






## Modelo 8: Jerárquico con vecindarios, tipo de edificio como covariable y pendientes, interceptos y varianzas diferentes

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
  summary_mod_8 <- read_rds("../out/models/model_01.rds")$BUGSoutput$summary  
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


rmse_train_8 <- mean(preds_8$res^2)
rmse_train_log_8 <- mean((preds_8$mean - log(preds_8$obs))^2)
rmse_test_8 <- mean(preds_test_8$res^2)
rmse_test_log_8 <- mean((preds_test_8$mean - log(preds_test_8$obs))^2)

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
