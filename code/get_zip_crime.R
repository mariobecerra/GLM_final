library(tidyverse)
library(rgdal)
library(maptools)
library(gpclib)
gpclibPermit() 

crime_data <- read.csv(unz("../data/NYPD_Complaint_Data_Historic.zip", "NYPD_Complaint_Data_Historic.csv"), stringsAsFactors = F)

nyc_zip_shape <- readOGR("../data/ZIP_CODE_040114/", layer = "ZIP_CODE_040114") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

coords <- data.frame(long = crime_data$Longitude, 
                     lat = crime_data$Latitude,
                     CMPLNT_NUM = crime_data$CMPLNT_NUM) %>% 
  filter(complete.cases(.))

get_info <- function(coords_df){
  info_df <- over(SpatialPoints(coords_df[,c("long", "lat")], 
       proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")), 
       nyc_zip_shape)
  out <- coords_df %>% 
    mutate(zip_code = info_df$ZIPCODE,
           CMPLNT_NUM = coords_df$CMPLNT_NUM)
  return(out)
}

info <- get_info(coords)

crimes <- crime_data %>% 
  left_join(info, by = "CMPLNT_NUM")

saveRDS(crimes, "../out/crimes.rds")

crimes_per_zip_code <- crimes %>% 
  group_by(zip_code) %>% 
  tally() %>% 
  ungroup() %>% 
  mutate(zip_code = as.character(zip_code)) %>% 
  left_join(nyc_zip_shape@data %>% 
              select(zip_code = ZIPCODE,
                     POPULATION)) %>% 
  mutate(n_crimes = n,
         crimes_pop = n_crimes/POPULATION)

saveRDS(crimes_per_zip_code, "../out/crimes_per_zip_code.rds")

