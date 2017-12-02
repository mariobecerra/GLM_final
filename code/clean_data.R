# clean data
library(stringi)
library(tidyverse)
if(!require(utilsMBC)) devtools::install_github("mariobecerra/utilsMBC")

# Clean neighborhood data

neighborhoods <- read_csv2("../data/nyc_zip_neighborhoods.csv")

zip_codes_list <- strsplit(neighborhoods$ZIP_Codes, ",")
names(zip_codes_list) <- neighborhoods$Neighborhood


zip_codes_tibble <- lapply(seq_along(zip_codes_list), function(i){
  neighborhood = names(zip_codes_list)[[i]]
  out <- tibble(zip_code = zip_codes_list[[i]]) %>% 
    mutate(Neighborhood = neighborhood)
  return(out)
}) %>% 
  bind_rows()

zip_code_neighborhood_borough <- neighborhoods %>% 
  select(Borough, Neighborhood) %>% 
  inner_join(zip_codes_tibble) %>% 
  mutate(zip_code = as.character(stri_replace_all(zip_code, replacement = "", fixed = " ")))

saveRDS(zip_code_neighborhood_borough, "../out/zip_code_neighborhood_borough.rds")

# Clean sales data

nyc_sales <- read_csv("../data/nyc-rolling-sales.csv",
                na = c("", "NA", "-"),
                col_types = list(
                  col_integer(),
                  col_integer(),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_integer(),
                  col_integer(),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_character(),
                  col_integer(),
                  col_integer(),
                  col_integer(),
                  col_integer(),
                  col_integer(),
                  col_integer(),
                  col_integer(),
                  col_integer(),
                  col_character(),
                  col_double(),
                  col_character()
                )) %>% 
  make_names() %>%
  filter(SALE_PRICE > 7000, 
         !is.na(SALE_PRICE),
         !is.na(LAND_SQUARE_FEET),
         LAND_SQUARE_FEET > 0,
         !is.na(GROSS_SQUARE_FEET),
         GROSS_SQUARE_FEET > 0,
         ZIP_CODE != 0) %>% 
  # mutate(BOROUGH_NAME = 
  #          ifelse(BOROUGH == 1, "Manhattan", ifelse(
  #            BOROUGH == 2, "Bronx", ifelse(
  #              BOROUGH == 3, "Brooklyn", ifelse(
  #                BOROUGH == 4, "Queens", "Staten Island")))
  #          ),
  #        Complete_address = paste0(
  #          ADDRESS, 
  #          ", ", 
  #          NEIGHBORHOOD,
  #          ", ",
  #          BOROUGH_NAME, 
  #          ", New York, ", 
  #          ZIP_CODE, 
  #          ", USA")) %>% 
  group_by(BUILDING_CLASS_CATEGORY) %>% 
  mutate(num = length(BUILDING_CLASS_CATEGORY)) %>% 
  filter(num > 300, 
         BUILDING_CLASS_CATEGORY != "22 STORE BUILDINGS") %>% 
  ungroup() %>% 
  select(-num) %>% 
  mutate(zip_code = as.character(ZIP_CODE),
         YEAR_BUILT = ifelse(YEAR_BUILT == 0, NA, YEAR_BUILT)) %>% 
  left_join(zip_code_neighborhood_borough) %>% 
  select(-ZIP_CODE, 
         -BOROUGH, 
         -NEIGHBORHOOD, 
         -APARTMENT_NUMBER, 
         -BLOCK, 
         -LOT, 
         -EASE_MENT, 
         -TAX_CLASS_AT_PRESENT)

saveRDS(nyc_sales, "../out/nyc_sales.rds")



