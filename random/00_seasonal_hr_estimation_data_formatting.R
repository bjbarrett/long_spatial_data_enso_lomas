library(tidyverse)
library(sf)
library(ctmm)
library(lubridate)
library(ctmmweb)
library(mapview)
library(ggmap)
library(raster)

# read in sleep site data and format for ctmm
slp <- readRDS("data/all_sleep_sites_sf.rds") %>% 
  dplyr::select(-n) %>% 
  mutate(month = month(date),
         year = year(date),
         week = week(date),
         season = ifelse(month %in% c(12, 1:4), "dry", "wet"),
         #season = ifelse(month %in% 1:6, "dry", "wet"),
         season_yr = str_c(season,"_", year), 
         yr_prev = as.character(as.numeric(year) -1), 
         yr_after = as.character(as.numeric(year) +1),
         season_idx = ifelse(as.numeric(month) %in% 5:11, 
                             season_yr,
                             ifelse(as.numeric(month)==12,
                                    str_c(season_yr,"/",yr_after),
                                    str_c(season,"_", yr_prev, "/", year))),
         #individual.local.identifier = str_c(group, season, year, sep = "_"),
         individual.local.identifier = str_c(group,"_",season_idx)) %>%  # start with annual home ranges
  distinct(group, timestamp, .keep_all = TRUE) %>% # remove duplicates
  st_transform(crs = "+proj=longlat +datum=WGS84 +no_defs") %>%
  bind_cols(.,st_coordinates(.) %>% as_tibble()) %>%
  as_tibble() %>%
  dplyr::select(-geometry) %>%
  dplyr::select(individual.local.identifier,
                timestamp,
                location.long = X,
                location.lat = Y,
                season,
                year,
                week) %>%
  arrange(individual.local.identifier, timestamp) 

# get unique weeks
slp_weeks <- slp %>% 
  group_by(individual.local.identifier) %>% 
  summarise(unique_weeks = length(unique(week))) %>% 
  filter(unique_weeks > 2) # must be 3 unique weeks or over 

# may want to increase the minimum number of unique weeks later
# however doing so removes potentially biologically important segments (e.g. CE 2015)

# remove segments with less than 3 unique weeks and convert to tele object
slp_tele <- slp %>% 
  semi_join(slp_weeks, by = "individual.local.identifier") %>% 
  as.telemetry(projection = "+proj=utm +zone=16 +north +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# remove segments with below 10 locations
# less than 10 effective sample size tends to be problematic (C. H. Fleming et al., 2018)
# changing to 8 for now ...
slp_tele_new <- slp_tele[sapply(slp_tele, nrow) >= 8] 

#.................................
# Calculate AKDEs --------------------------
#......................

## batch calculation ## 

SVF <- FITS <- AKDE <- list()

tictoc::tic()
for(i in 1:length(slp_tele_new)){
  SVF[[i]] <- variogram(slp_tele_new[[i]], dt = c(1,10) %#% "hour")
  GUESS <- ctmm.guess(slp_tele_new[[i]],interactive=FALSE, variogram = SVF[[i]])
  FITS[[i]] <- ctmm.select(slp_tele_new[[i]],GUESS,trace=2)
  AKDE[[i]] <- akde(slp_tele_new[[i]],FITS[[i]], weights = TRUE, grid=list(dr=10,align.to.origin=TRUE))
}
tictoc::toc() 

names(AKDE) <- names(FITS)  <- names(SVF) <- names(slp_tele_new)

#.........................
# Mapping -------
##
# need to check visually if home ranges make sense

## get AKDEs
UD <- AKDE

# change UD list to sf and lat long coords
UD_sf <- UD  %>% 
  purrr::map(ctmm::as.sf, level.UD = 0.98) %>% # 98% UD most closely resembles 95% GPS UD
  reduce(rbind) %>% 
  mutate(id = rep(names(UD), each = 3),
         group = str_sub(id,1,2),
         season = str_sub(id,4,6),
         year = str_remove(id, "[^_]*_[^_]*_")) %>% 
  filter(str_detect(row.names(.), "est")) %>% 
  dplyr::select(id, group,year,season, geometry) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84")

# get CIs
CI_sf <-  UD  %>% 
  purrr::map(ctmm::as.sf, level.UD = 0.98) %>% 
  reduce(rbind) %>% 
  mutate(id = rep(names(UD), each = 3),
         group = str_sub(id,1,2),
         season = str_sub(id,4,6),
         year = str_remove(id, "[^_]*_[^_]*_")) %>%  
  filter(!str_detect(row.names(.), "est")) %>% 
  dplyr::select(id,group,year, season, geometry) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84")

# change data list to sf and lat long coords
DATA_sf <- slp_tele_new  %>%
  purrr::map(ctmm::as.sf) %>% 
  reduce(rbind) %>% 
  mutate(id = identity,
         group = str_sub(id,1,2),
         season = str_sub(id,4,6),
         year = str_remove(id, "[^_]*_[^_]*_")) %>% 
  dplyr::select(id,group, year,season, geometry) %>% 
  st_transform(crs = "+proj=longlat +datum=WGS84")
#................
#
# MAPVIEW
#
mapview::mapviewOptions(fgb = FALSE) # prevents mapview from throwing weird error when using burst

groups <- unique(DATA_sf$group)
# for(i in groups){
#   print(mapview(DATA_sf[DATA_sf$group==i,] , zcol="id" , burst = T, map.types="Esri.WorldImagery") +
#           mapview(UD_sf[UD_sf$group==i,] , zcol="id", burst = T) +
#           mapview(CI_sf[CI_sf$group==i,] , zcol="id", burst = T ))
# }

mapview(DATA_sf[DATA_sf$group=="aa",] , zcol="id" , burst = T) +
  mapview(UD_sf[UD_sf$group=="aa",] , zcol="id", burst = T, alpha.regions = 0.1) +
  mapview(CI_sf[CI_sf$group=="aa",] , zcol="id", burst = T , alpha.regions = 0)

# save
saveRDS(SVF, "Intermediate/ctmm/HR/slp_1990-2019_SVF_seasonal.rds")
saveRDS(FITS, "Intermediate/ctmm/HR/slp_1990-2019_FITs_seasonal.rds")
saveRDS(UD, "Intermediate/ctmm/HR/slp_1990-2019_AKDEs_seasonal.rds")
saveRDS(slp_tele_new, "Intermediate/ctmm/HR/slp_1990-2019_DATA_seasonal.rds")

#.......................
#
## Make dataframe with areas and group sizes-----
#

# read in data
DATA <- readRDS("Intermediate/ctmm/HR/slp_1990-2019_DATA_seasonal.rds")
#UD <- readRDS("Intermediate/ctmm/HR/slp_1990-2019_RSF_AKDEs_seasonal.rds")
UD <- readRDS("Intermediate/ctmm/HR/slp_1990-2019_AKDEs_seasonal.rds")

# get group size data
GS <- read.csv("Data/annual_group_sizes.csv") %>% 
  mutate(group_year = str_c(group,year,sep = "_")) 

## functions to get HR areas

# function to get summary information from AKDEs
summarize_akde <- function(akde){
  
  summary <- summary(akde, units = FALSE, level.UD = 0.98) 
  
  tibble(id = akde@info$identity,
         low_area = (summary$CI[1])/1000000, # convert m2 to km2
         mean_area = (summary$CI[2])/1000000,
         high_area = (summary$CI[3])/1000000,
         DOF = summary$DOF[1])
}

# wrapper to stack area info into data frame
make_df <- function(id){
  map_dfr(id, summarize_akde) 
}

# execute functions to make data frame and combine group sizes
df <- make_df(UD) %>%
  mutate(sd_area = (high_area - low_area)/3.92, # convert non-gaussian (gamma?) CIs to Gaussian standard deviation
         season = str_sub(id, 4,6),
         group_year = ifelse(season == "wet",
                             str_c(str_sub(id, 1,2), 
                                   str_sub(id,8,11), 
                                   sep = "_"),
                             str_c(str_sub(id, 1,2), 
                                   as.character(as.numeric(str_sub(id,8,11)) + 1), 
                                   sep = "_"))) %>%   
  left_join(GS, by = "group_year") %>% 
  dplyr::select(id, low_area,mean_area,high_area, sd_area, DOF, group_size)

saveRDS(df, "data/df_slpHRarea_seasonal.rds")
write.csv(df, "data/df_slpHRarea_seasonal.csv")