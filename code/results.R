library(gridExtra)
library(tidyverse)
options(scipen=999)
theme_set(theme_bw())
#theme_set(theme_bw(base_size = 20))

######################################################
######################################################
### Funciones
######################################################
######################################################

plot_residuals <- function(pred, obs){
  tibble(pred = pred, obs = obs) %>% 
    mutate(ix = 1:nrow(.),
           res = obs - pred) %>% 
    ggplot() +
    geom_point(aes(ix, res), size = 0.8, alpha = 0.5) +
    xlab("Índice") +
    ylab("Residual")
}


plot_residuals_train_test <- function(pred_train, obs_train, pred_test, obs_test){
  tibble(pred = pred_train, obs = obs_train) %>% 
    mutate(
      set = "Entrenamiento",
      ix = 1:nrow(.),
      res = obs - pred
    ) %>% 
    bind_rows(
      tibble(pred = pred_test, obs = obs_test) %>% 
        mutate(
          set = "Prueba",
          ix = 1:nrow(.),
          res = obs - pred
        )
    ) %>% 
    ggplot() +
    geom_point(aes(ix, res), size = 0.8, alpha = 0.5) +
    facet_wrap(~set, scales = "free_x") +
    xlab("Índice") +
    ylab("Residual")
}

plot_obs_vs_pred <- function(pred_train, pred_test){
  pred_train %>% 
    select(obs, mean) %>% 
    mutate(set = "Entrenamiento") %>% 
    bind_rows(
      pred_test %>% 
        select(obs, mean) %>% 
        mutate(set = "Prueba")
    ) %>% 
    ggplot() +
    geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.7) +
    geom_abline(slope = 1) +
    facet_wrap(~set) +
    xlab("Observado") +
    ylab("Estimado")
}

rhat_statistic_params <- function(summary_mod){
  sum_df <-summary_mod %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    filter(!grepl("yf", rowname)) %>% 
    mutate(ix = 1:nrow(.))
  if(nrow(sum_df) > 30){
    gg <- sum_df %>% 
      ggplot() + 
      geom_point(aes(ix, Rhat))
  } else {
    gg <- sum_df %>% 
      ggplot() + 
      geom_point(aes(rowname, Rhat))
  }
  gg_out <- gg +
    geom_hline(yintercept = 1.2, 
               linetype = 'dashed', 
               size = 1, 
               color = 'black', 
               alpha = 0.6) +
    expand_limits(y = 1) +
    xlab("Parámetro")
  return(gg_out)
}

rhat_statistic_yf <- function(summary_mod){
  gg <- grid.arrange(
    summary_mod %>% 
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
      expand_limits(y = 1) +
      xlab("Índice") +
      ggtitle("Entrenamiento"),
    summary_mod %>% 
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
      expand_limits(y = 1) +
      xlab("Índice") +
      ggtitle("Prueba"),
    ncol = 2
  )
  return(gg)
}

effective_sample_size_params <- function(summary_mod, nreal = 4000){
  sum_df <- summary_mod %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    filter(!grepl("yf", rowname)) %>% 
    mutate(ix = 1:nrow(.))
  if(nrow(sum_df) > 30){
    gg <- sum_df %>% 
      ggplot() + 
      geom_point(aes(ix, n.eff))
  } else {
    gg <- sum_df %>% 
      ggplot() + 
      geom_point(aes(rowname, n.eff))
  }
  gg_out <- gg +
    geom_hline(yintercept = nreal, 
               color = 'grey', linetype = 'dashed') +
    xlab("Parámetro") +
    ylab("Tamaño de muestra efectivo") +
    expand_limits(y = 0)
  return(gg_out)
}


effective_sample_size_yf <- function(summary_mod, nreal = 4000){
  gg_out <- grid.arrange(
    summary_mod %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(grepl("yf", rowname)) %>% 
      filter(!grepl("test", rowname)) %>% 
      mutate(ix = 1:nrow(.)) %>% 
      ggplot() + 
      geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
      geom_hline(yintercept = nreal, 
                 color = 'grey', linetype = 'dashed') +
      xlab("Índice") +
      ylab("Tamaño de muestra efectivo") +
      ggtitle("Entrenamiento") +
      expand_limits(y = 0),
    
    summary_mod %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(grepl("yf", rowname)) %>% 
      filter(grepl("test", rowname)) %>% 
      mutate(ix = 1:nrow(.)) %>% 
      ggplot() + 
      geom_point(aes(ix, n.eff), size = 0.4, alpha = 0.7) +
      geom_hline(yintercept = nreal, 
                 color = 'grey', linetype = 'dashed') +
      xlab("Índice") +
      ylab("") +
      ggtitle("Prueba") +
      expand_limits(y = 0),
    ncol = 2
  ) 
  return(gg_out)
}




######################################################
######################################################
### Datos
######################################################
######################################################

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

######################################################
######################################################
### Gráficas para EDA
######################################################
######################################################

grid.arrange(nyc_sales %>% 
               ggplot() + 
               geom_histogram(aes(SALE_PRICE)) + 
               labs(x = "Precio de venta", y = "Frecuencia"), 
             nyc_sales %>% 
               ggplot() + 
               geom_histogram(aes(log(SALE_PRICE))) + 
               labs(x = "Logaritmo del precio de venta", y = "Frecuencia"), 
             ncol=2) %>% 
  ggsave(., filename = "../out/plots/eda_histograma_precio_venta.pdf", 
         device = "pdf", width = 200, height = 100, units = "mm")

grid.arrange(nyc_sales %>% 
               ggplot() + 
               geom_histogram(aes(GROSS_SQUARE_FEET)) + 
               labs(x = "Superficie de construcción", y = "Frecuencia"), 
             nyc_sales %>% 
               ggplot() + 
               geom_histogram(aes(log(GROSS_SQUARE_FEET))) + 
               labs(x = "Superficie de construcción", y = "Frecuencia")  , 
             ncol=2) %>% 
  ggsave(., filename = "../out/plots/eda_histograma_superficie.pdf", 
         device = "pdf", width = 200, height = 70, units = "mm")



grid.arrange(nyc_sales %>% 
               ggplot() + 
               geom_histogram(aes(LAND_SQUARE_FEET)) + labs(x = "Superficie de terreno ", y = "Frecuencia"), 
             nyc_sales %>% 
               ggplot() + 
               geom_histogram(aes(log(LAND_SQUARE_FEET))) + 
               labs(x = "Superficie", y = "Frecuencia"), 
             ncol=2) %>% 
  ggsave(., filename = "../out/plots/eda_histograma_superficie_total_land.pdf", 
         device = "pdf", width = 200, height = 70, units = "mm")

(nyc_sales %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), size = 0.4, alpha = 0.5) + 
  labs(x = "Logaritmo de superficie de construcción", y = "Logaritmo de precio de venta")) %>% 
  ggsave(., filename = "../out/plots/eda_dispersion_superficie_vs_precio.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

(nyc_sales %>% 
  ggplot() +
  geom_point(aes(log(LAND_SQUARE_FEET), log(SALE_PRICE)), size = 0.4, alpha = 0.5) + 
  labs(x = "Logaritmo de superficie de terreno", y = "Logaritmo de precio de venta")) %>% 
  ggsave(., filename = "../out/plots/eda_dispersion_superficie_total_vs_precio.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


(nyc_sales %>% 
  ggplot() +
  geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), size = 0.4, alpha = 0.5) + 
  labs(x = "Logaritmo de superficie de construcción", y = "Logaritmo de superficie de terreno")) %>% 
  ggsave(., filename = "../out/plots/eda_dispersion_superficie_total_vs_superficie.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

(nyc_sales %>% 
  ggplot() +
  geom_histogram(aes(x = log(SALE_PRICE), y = ..density..)) +
  facet_wrap(~Borough) + 
  labs(x = "Logaritmo de precio de venta", y = "Densidad")) %>% 
  ggsave(., filename = "../out/plots/eda_histogram_price_borough.pdf", 
         device = "pdf", width = 200, height = 150, units = "mm")

(nyc_sales %>% 
  ggplot(aes(x = Borough, y = log(SALE_PRICE))) +
  geom_boxplot() + 
  stat_summary(fun.y = mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
               width = .75, linetype = "dashed") + 
  labs(x = "", y = "Logaritmo de precio de venta")) %>% 
  ggsave(., filename = "../out/plots/eda_boxplot_price_borough.pdf", 
         device = "pdf", width = 150, height = 60, units = "mm")


(nyc_sales %>% 
  mutate(PRICE_PER_SQ_FT = SALE_PRICE/GROSS_SQUARE_FEET) %>% 
  ggplot(aes(x = log(PRICE_PER_SQ_FT), y = ..density..)) +
  geom_histogram() + 
  facet_wrap(~Borough) + 
    labs(x= "Precio x pie cuadrado" , y = "Densidad")) %>% 
  ggsave(., filename = "../out/plots/eda_histogram_precio_pie_cuad_borough.pdf", 
         device = "pdf", width = 200, height = 150, units = "mm")


(nyc_sales %>% 
    mutate(PRICE_PER_SQ_FT = SALE_PRICE/GROSS_SQUARE_FEET) %>% 
    ggplot(aes(x = Borough, y = log(PRICE_PER_SQ_FT))) +
    geom_boxplot() + 
    labs(x= "Precio x pie cuadrado" , y = "Densidad")) %>% 
  ggsave(., filename = "../out/plots/eda_boxplot_precio_pie_cuad_borough.pdf", 
         device = "pdf", width = 200, height = 150, units = "mm")

(nyc_sales %>% 
    mutate(Neighborhood2 = paste(Borough,"\n", Neighborhood)) %>% 
    ggplot() +
    geom_point(aes(log(GROSS_SQUARE_FEET), log(SALE_PRICE)), size = 0.3, alpha = 0.6) +
    facet_wrap(~ Neighborhood2) + 
    theme_bw(base_size = 6) +
    labs(x = "Superficie total" , y = "Precio de venta")) %>% 
  ggsave(., filename = "../out/plots/eda_scatter_by_neighborhood.pdf", 
         device = "pdf", width = 200, height = 250, units = "mm")




######################################################
######################################################
### Modelo de unidades iguales (complete pooling)
######################################################
######################################################

summary_mod_comp_pooling <- read_rds("../out/models/summary_mod_comp_pooling.rds")

### Predictions for train and test set
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

### Summary of parameters
summary_mod_comp_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

# Percentage of observations inside the 95% probability interval
(preds_comp_pooling %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_comp_pooling)

(preds_test_comp_pooling %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_comp_pooling)

# RMSEs
(rmse_train_comp_pooling <- sqrt(mean(preds_comp_pooling$res^2)))
(rmse_test_comp_pooling <- sqrt(mean(preds_test_comp_pooling$res^2)))

# MAEs
(mae_train_comp_pooling <- mean(abs(preds_comp_pooling$res)))
(mae_test_comp_pooling <- mean(abs(preds_test_comp_pooling$res)))



###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

rhat_statistic_params(summary_mod_comp_pooling) %>% 
  ggsave(., filename = "../out/plots/comp_pooling_r_statistic_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

rhat_statistic_yf(summary_mod_comp_pooling) %>% 
  ggsave(., filename = "../out/plots/comp_pooling_r_statistic_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

# Effective sample size

effective_sample_size_params(summary_mod_comp_pooling, 4000) %>% 
  ggsave(., filename = "../out/plots/comp_pooling_n_eff_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

effective_sample_size_yf(summary_mod_comp_pooling, 4000) %>% 
  ggsave(., filename = "../out/plots/comp_pooling_n_eff_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

### Plot residuals
plot_residuals_train_test(preds_comp_pooling$mean, 
                          log(preds_comp_pooling$obs), 
                          preds_test_comp_pooling$mean, 
                          log(preds_test_comp_pooling$obs)) %>% 
  ggsave(., 
         file = "../out/plots/comp_pooling_resids.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


### Observado contra ajustado
plot_obs_vs_pred(preds_comp_pooling, preds_test_comp_pooling) %>% 
  ggsave(., 
         file = "../out/plots/comp_pooling_obs_vs_pred.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


###
# Obs vs pred by neighborhood and zip code
##

(preds_comp_pooling %>% 
    mutate(zip_code = paste(substr(nyc_train$Borough, 1, 2), 
                            nyc_train$zip_code_int)) %>% 
    ggplot() +
    geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
    geom_abline(slope = 1) +
    facet_wrap(~zip_code) +
    theme_bw(base_size = 7)) %>% 
  ggsave(., filename = "../out/plots/comp_pooling_obs_vs_pred_train_zip.pdf", 
         device = 'pdf', width = 200, height = 260, units = "mm")

preds_test_comp_pooling %>% 
  mutate(Neighborhood = paste(substr(nyc_test$Borough, 1, 2), nyc_test$Neighborhood)) %>% 
  ggplot() +
  geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
  geom_abline(slope = 1) +
  facet_wrap(~Neighborhood)

(preds_test_comp_pooling %>% 
    mutate(zip_code = paste(substr(nyc_test$Borough, 1, 2), nyc_test$zip_code_int)) %>% 
    ggplot() +
    geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
    geom_abline(slope = 1) +
    facet_wrap(~zip_code) +
    theme_bw(base_size = 7)) %>% 
  ggsave(., filename = "../out/plots/comp_pooling_obs_vs_pred_test_zip.pdf", 
         device = 'pdf', width = 200, height = 260, units = "mm")



# preds_test_comp_pooling %>% 
#   mutate(Neighborhood = paste(substr(nyc_test$Borough, 1, 2), nyc_test$Neighborhood),
#          x = log(nyc_test$GROSS_SQUARE_FEET)) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
#   geom_abline(slope = 1) +
#   facet_wrap(~Neighborhood)
# 
# preds_test_comp_pooling %>%
#   ggplot(aes(log(obs), mean)) +
#   geom_point(alpha = 0.4, size = 0.4) +
#   geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), alpha = 0.4) +
#   geom_abline(slope = 1)
# 
# preds_comp_pooling %>% 
#   mutate(Neighborhood = paste(nyc_train$Borough, nyc_train$Neighborhood),
#          BUILDING_CLASS_CATEGORY = nyc_train$BUILDING_CLASS_CATEGORY) %>% 
#   ggplot(aes(x = log(obs), y = X50., color = BUILDING_CLASS_CATEGORY)) +
#   geom_point(alpha = 0.6) + geom_errorbar(aes(ymin = X2.5., ymax = X97.5.)) +
#   geom_abline(slope = 1) +
#   facet_wrap(~Neighborhood)
# 
# preds_test_comp_pooling %>% 
#   mutate(Neighborhood = paste(nyc_test$Borough, nyc_test$Neighborhood),
#          BUILDING_CLASS_CATEGORY = nyc_test$BUILDING_CLASS_CATEGORY) %>% 
#   ggplot() +
#   geom_point(aes(log(obs), (mean), color = BUILDING_CLASS_CATEGORY), alpha = 0.6) +
#   geom_abline(slope = 1) +
#   facet_wrap(~Neighborhood)


# (summary_mod_three_levels %>%
#     as.data.frame() %>%
#     rownames_to_column() %>%
#     filter(grepl("alpha[", rowname, fixed = T)) %>%
#     set_names(paste0("alpha_", make.names(names(.)))) %>%
#     mutate(zip_code_int = 1:nrow(.)) %>%
#     bind_cols(
#       summary_mod_three_levels %>%
#         as.data.frame() %>%
#         rownames_to_column() %>%
#         filter(grepl("beta_1[", rowname, fixed = T)) %>%
#         set_names(paste0("beta_", make.names(names(.))))
#     ) %>%
#     full_join(
#       nyc_train %>%
#         mutate(
#           x = log(GROSS_SQUARE_FEET),
#           y = log(SALE_PRICE)) %>%
#         select(x, y, zip_code, zip_code_int, Borough, Neighborhood)
#     ) %>%
#     mutate(yhat = alpha_mean + beta_mean*x) %>%
#     ggplot(aes(x, y)) +
#     geom_point(size = 0.4, alpha = 0.4) +
#     geom_line(aes(y = yhat, group = zip_code)) +
#     facet_wrap(~zip_code) + theme_bw(base_size = 5)) %>% 
#   ggsave(., filename = "~/Desktop/aaa.pdf",
#          device = 'pdf', width = 200, height = 260, units = "mm")






######################################################
######################################################
### Modelo de unidades distintas (no pooling)
######################################################
######################################################

summary_mod_no_pooling <- read_rds("../out/models/summary_mod_no_pooling.rds")


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


# RMSEs
(rmse_train_no_pooling <- sqrt(mean(preds_no_pooling$res^2)))
(rmse_test_no_pooling <- sqrt(mean(preds_test_no_pooling$res^2)))

# MAEs
(mae_train_no_pooling <- mean(abs(preds_no_pooling$res)))
(mae_test_no_pooling <- mean(abs(preds_test_no_pooling$res)))



###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

rhat_statistic_params(summary_mod_no_pooling) %>% 
  ggsave(., filename = "../out/plots/no_pooling_r_statistic_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

rhat_statistic_yf(summary_mod_no_pooling) %>% 
  ggsave(., filename = "../out/plots/no_pooling_r_statistic_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

# Effective sample size

effective_sample_size_params(summary_mod_no_pooling, 4000) %>% 
  ggsave(., filename = "../out/plots/no_pooling_n_eff_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

effective_sample_size_yf(summary_mod_no_pooling, 4000) %>% 
  ggsave(., filename = "../out/plots/no_pooling_n_eff_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

### Plot residuals
plot_residuals_train_test(preds_no_pooling$mean, 
                          log(preds_no_pooling$obs), 
                          preds_test_no_pooling$mean, 
                          log(preds_test_no_pooling$obs)) %>% 
  ggsave(., 
         file = "../out/plots/no_pooling_resids.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


### Observado contra ajustado
plot_obs_vs_pred(preds_no_pooling, preds_test_no_pooling) %>% 
  ggsave(., 
         file = "../out/plots/no_pooling_obs_vs_pred.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

(preds_test_no_pooling %>% 
    mutate(zip_code = paste(substr(nyc_test$Borough, 1, 2), nyc_test$zip_code_int)) %>% 
    ggplot() +
    geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
    geom_abline(slope = 1) +
    facet_wrap(~zip_code) +
    theme_bw(base_size = 7)) %>% 
  ggsave(., filename = "../out/plots/no_pooling_obs_vs_pred_test_zip.pdf", 
         device = 'pdf', width = 200, height = 260, units = "mm")

# Parámetros
(summary_mod_no_pooling %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  filter(grepl("alpha[", rowname, fixed = T) |
           grepl("beta[", rowname, fixed = T)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(params = ifelse(grepl("alpha", rowname), "alpha", "beta")) %>% 
  group_by(params) %>% 
  mutate(ix = 1:n()) %>% 
  ggplot(aes(x = ix, y = mean)) +
  geom_point(size = 0.7) +
  geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), size = 0.3, alpha = 0.7) +
  xlab("Índice") +
  ylab("Valor") +
  facet_wrap(~params, scales = "free")) %>%  
  ggsave(., 
         file = "../out/plots/no_pooling_param_values.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")



# (summary_mod_no_pooling %>%
#     as.data.frame() %>%
#     rownames_to_column() %>%
#     filter(grepl("alpha[", rowname, fixed = T)) %>%
#     set_names(paste0("alpha_", make.names(names(.)))) %>%
#     mutate(zip_code_int = 1:nrow(.)) %>%
#     bind_cols(
#       summary_mod_no_pooling %>%
#         as.data.frame() %>%
#         rownames_to_column() %>%
#         filter(grepl("beta[", rowname, fixed = T)) %>%
#         set_names(paste0("beta_", make.names(names(.))))
#     ) %>%
#     full_join(
#       nyc_train %>%
#         mutate(
#           x = log(GROSS_SQUARE_FEET),
#           y = log(SALE_PRICE)) %>%
#         select(x, y, zip_code, zip_code_int, Borough, Neighborhood)
#     ) %>%
#     mutate(yhat = alpha_mean + beta_mean*x) %>%
#     ggplot(aes(x, y)) +
#     geom_point(size = 0.4, alpha = 0.4) +
#     geom_line(aes(y = yhat, group = zip_code)) +
#     facet_wrap(~zip_code) + theme_bw(base_size = 5)) %>%
#   ggsave(., filename = "~/Desktop/aaa2.pdf",
#          device = 'pdf', width = 200, height = 260, units = "mm")



######################################################
######################################################
### Partial pooling, three levels: borough, neighborhood & zip code
######################################################
######################################################

summary_mod_three_levels <- read_rds("../out/models/summary_mod_three_levels.rds")


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


# RMSEs
(rmse_train_three_levels <- sqrt(mean(preds_three_levels$res^2)))
(rmse_test_three_levels <- sqrt(mean(preds_test_three_levels$res^2)))

# MAEs
(mae_train_three_levels <- mean(abs(preds_three_levels$res)))
(mae_test_three_levels <- mean(abs(preds_test_three_levels$res)))



###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

rhat_statistic_params(summary_mod_three_levels) %>% 
  ggsave(., filename = "../out/plots/three_levels_r_statistic_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

rhat_statistic_yf(summary_mod_three_levels) %>% 
  ggsave(., filename = "../out/plots/three_levels_r_statistic_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

# Effective sample size

effective_sample_size_params(summary_mod_three_levels, 8000) %>% 
  ggsave(., filename = "../out/plots/three_levels_n_eff_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

effective_sample_size_yf(summary_mod_three_levels, 8000) %>% 
  ggsave(., filename = "../out/plots/three_levels_n_eff_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


### Plot residuals
plot_residuals_train_test(preds_three_levels$mean, 
                          log(preds_three_levels$obs), 
                          preds_test_three_levels$mean, 
                          log(preds_test_three_levels$obs)) %>% 
  ggsave(., 
         file = "../out/plots/three_levels_resids.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


### Observado contra ajustado
plot_obs_vs_pred(preds_three_levels, preds_test_three_levels) %>% 
  ggsave(., 
         file = "../out/plots/three_levels_obs_vs_pred.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

(preds_test_three_levels %>% 
    mutate(zip_code = paste(substr(nyc_test$Borough, 1, 2), nyc_test$zip_code_int)) %>% 
    ggplot() +
    geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
    geom_abline(slope = 1) +
    facet_wrap(~zip_code) +
    theme_bw(base_size = 7)) %>% 
  ggsave(., filename = "../out/plots/three_levels_obs_vs_pred_test_zip.pdf", 
         device = 'pdf', width = 200, height = 260, units = "mm")

# Parámetros
(summary_mod_three_levels %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    filter(!grepl("yf", rowname)) %>% 
    filter(grepl("alpha[", rowname, fixed = T) |
             grepl("beta_1", rowname, fixed = T)) %>% 
    set_names(make.names(names(.))) %>% 
    mutate(params = ifelse(grepl("alpha", rowname), "alpha", "beta")) %>% 
    group_by(params) %>% 
    mutate(ix = 1:n()) %>% 
    ggplot(aes(x = ix, y = mean)) +
    geom_point(size = 0.7) +
    geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), size = 0.3, alpha = 0.7) +
    xlab("Índice") +
    ylab("Valor") +
    facet_wrap(~params, scales = "free")) %>%  
  ggsave(., 
         file = "../out/plots/three_levels_param_values.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("alpha[", rowname, fixed = T)) %>% 
  set_names(paste0("alpha_", make.names(names(.)))) %>% 
  mutate(zip_code_int = 1:nrow(.)) %>% 
  bind_cols(
    summary_mod_three_levels %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(grepl("beta_1[", rowname, fixed = T)) %>% 
      set_names(paste0("beta_", make.names(names(.))))
  ) %>% 
  full_join(
    nyc_train %>% 
      mutate(
        x = log(GROSS_SQUARE_FEET),
        y = log(SALE_PRICE)) %>% 
      select(x, y, zip_code, zip_code_int, Borough, Neighborhood)
  ) %>% 
  mutate(yhat = alpha_mean + beta_mean*x) %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.4, alpha = 0.4) +
  geom_line(aes(y = yhat, group = zip_code)) +
  facet_wrap(~Neighborhood)


summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(grepl("alpha[", rowname, fixed = T)) %>% 
  set_names(paste0("alpha_", make.names(names(.)))) %>% 
  mutate(zip_code_int = 1:nrow(.)) %>% 
  bind_cols(
    summary_mod_three_levels %>% 
      as.data.frame() %>% 
      rownames_to_column() %>% 
      filter(grepl("beta_1[", rowname, fixed = T)) %>% 
      set_names(paste0("beta_", make.names(names(.))))
  ) %>% 
  full_join(
    nyc_train %>% 
      mutate(
        x = log(GROSS_SQUARE_FEET),
        y = log(SALE_PRICE)) %>% 
      select(x, y, zip_code, zip_code_int, Borough, Neighborhood)
  ) %>% 
  mutate(yhat = alpha_mean + beta_mean*x) %>% 
  ggplot(aes(x, y)) +
  geom_point(size = 0.4, alpha = 0.4) +
  geom_line(aes(y = yhat, group = zip_code)) +
  facet_wrap(~zip_code)





######################################################
######################################################
### Partial pooling, three levels: borough, neighborhood & zip code.
### t-distribugted response variable
######################################################
######################################################

summary_mod_tdist_three_levels <- read_rds("../out/models/summary_mod_tdist_three_levels.rds")


summary_mod_tdist_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname))

preds_tdist_three_levels <- summary_mod_tdist_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("yf", rowname)) %>% 
  filter(!grepl("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_train$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

preds_test_tdist_three_levels <- summary_mod_tdist_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  slice(grep("test", rowname)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(obs = nyc_test$SALE_PRICE,
         adj = exp(mean)) %>% 
  mutate(res = obs - exp(mean))

# Percentage of observations inside the 95% probability interval
(preds_tdist_three_levels %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_tdist_three_levels)

(preds_test_tdist_three_levels %>% 
    mutate(in_interval = (log( obs) >= X2.5. & log( obs) <= X97.5.)) %>% 
    .$in_interval %>% 
    sum())/nrow(preds_test_tdist_three_levels)


(rmse_train_tdist_three_levels <- sqrt(mean(preds_tdist_three_levels$res^2)))
(rmse_test_tdist_three_levels <- sqrt(mean(preds_test_tdist_three_levels$res^2)))



###########################
## Convergence diagnostics
###########################

## Gelman and Rubin R statistic

rhat_statistic_params(summary_mod_tdist_three_levels) %>% 
  ggsave(., filename = "../out/plots/tdist_three_levels_r_statistic_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

rhat_statistic_yf(summary_mod_tdist_three_levels) %>% 
  ggsave(., filename = "../out/plots/tdist_three_levels_r_statistic_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

# Effective sample size

effective_sample_size_params(summary_mod_tdist_three_levels, 8000) %>% 
  ggsave(., filename = "../out/plots/tdist_three_levels_n_eff_params.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

effective_sample_size_yf(summary_mod_tdist_three_levels, 8000) %>% 
  ggsave(., filename = "../out/plots/tdist_three_levels_n_eff_yf.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")



### Plot residuals
plot_residuals_train_test(preds_tdist_three_levels$mean, 
                          log(preds_tdist_three_levels$obs), 
                          preds_test_tdist_three_levels$mean, 
                          log(preds_test_tdist_three_levels$obs)) %>% 
  ggsave(., 
         file = "../out/plots/tdist_three_levels_resids.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")


### Observado contra ajustado
plot_obs_vs_pred(preds_tdist_three_levels, preds_test_tdist_three_levels) %>% 
  ggsave(., 
         file = "../out/plots/tdist_three_levels_obs_vs_pred.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

(preds_test_tdist_three_levels %>% 
    mutate(zip_code = paste(substr(nyc_test$Borough, 1, 2), nyc_test$zip_code_int)) %>% 
    ggplot() +
    geom_point(aes(log(obs), mean), alpha = 0.6, size = 0.6) +
    geom_abline(slope = 1) +
    facet_wrap(~zip_code) +
    theme_bw(base_size = 7)) %>% 
  ggsave(., filename = "../out/plots/tdist_three_levels_obs_vs_pred_test_zip.pdf", 
         device = 'pdf', width = 200, height = 260, units = "mm")

# Parámetros
(summary_mod_tdist_three_levels %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    filter(!grepl("yf", rowname)) %>% 
    filter(grepl("alpha[", rowname, fixed = T) |
             grepl("beta_1", rowname, fixed = T)) %>% 
    set_names(make.names(names(.))) %>% 
    mutate(params = ifelse(grepl("alpha", rowname), "alpha", "beta")) %>% 
    group_by(params) %>% 
    mutate(ix = 1:n()) %>% 
    ggplot(aes(x = ix, y = mean)) +
    geom_point(size = 0.7) +
    geom_errorbar(aes(ymin = X2.5., ymax = X97.5.), size = 0.3, alpha = 0.7) +
    xlab("Índice") +
    ylab("Valor") +
    facet_wrap(~params, scales = "free")) %>%  
  ggsave(., 
         file = "../out/plots/tdist_three_levels_param_values.pdf", 
         device = "pdf", width = 150, height = 80, units = "mm")

########
# DICs
########

readRDS("../out/models/model_comp_pooling.rds")$BUGSoutput$DIC
gc()
readRDS("../out/models/model_no_pooling.rds")$BUGSoutput$DIC
gc()
readRDS("../out/models/model_three_levels.rds")$BUGSoutput$DIC
gc()

# > readRDS("../out/models/model_comp_pooling.rds")$BUGSoutput$DIC
# [1] 630996.6
# > readRDS("../out/models/model_no_pooling.rds")$BUGSoutput$DIC
# [1] 7239403
# > readRDS("../out/models/model_three_levels.rds")$BUGSoutput$DIC
# [1] 16716.46


########
## Maps
#######

library(rgdal)
library(maptools)
library(gpclib)
gpclibPermit() 


nyc_zip_shape <- readOGR("../data/ignore/ZIP_CODE_040114/", layer = "ZIP_CODE_040114") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

nyc_zip_shape@data$id <- as.character(1:nrow(nyc_zip_shape@data))

nyc_zip_tibble <- fortify(nyc_zip_shape, region = "id") %>% 
  as_tibble() %>% 
  left_join(nyc_zip_shape@data)

nyc_zip_tibble %>% 
  ggplot()+
  geom_path(aes(x = long, y = lat, group = group),
            color = 'black', size = 0.1) +
  coord_fixed() +
  theme_bw()

alphas_zips <- summary_mod_three_levels %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  filter(!grepl("yf", rowname)) %>% 
  filter(grepl("alpha[", rowname, fixed = T)) %>% 
  set_names(make.names(names(.))) %>% 
  mutate(zip_code_int = 1:nrow(.)) %>% 
  left_join(
    nyc_sales %>% 
      select(ZIPCODE = zip_code, zip_code_int) %>% 
      unique(.)
    )

nyc_zip_tibble %>% 
  left_join(alphas_zips) %>% 
  ggplot()+
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean),
            color = 'black', size = 0.1) +
  coord_fixed() +
  theme_bw()




