library(rethinking)
library(janitor)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(rsoi)
library(ctmm)
library(stringr)

elcol_pal <- rev(brewer.pal(3 , "RdYlBu"))
group_pal <- brewer.pal(11 , "Spectral")

##group size
d_hr_gs <- read.csv("data/df_slpHRarea_group_size.csv")
str(d_hr_gs)
d_hr_gs$group_index <- as.integer(as.factor(d_hr_gs$group))
d_hr_gs$group_size_std <- standardize(d_hr_gs$group_size)


#get enso data
mei <- clean_names(download_mei())
d_mei <- mei[mei$year >= min(d_hr_gs$year) - 1,]
d_mei <- d_mei[complete.cases(d_mei),]
plot(mei~date , data=d_mei)
#combie data frames, will do posterior across time series later
str(d_hr_gs)
str(d_mei)
elcol_pal <- rev(brewer.pal(3 , "RdYlBu"))
group_pal <- brewer.pal(11 , "Spectral")

d_mei$phase_index <- as.integer(d_mei$phase)
plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" , cex=0.5)
mei_spl <- with(d_mei, smooth.spline(date, mei))
lines(mei_spl, col = "grey3")
abline(v=d_mei$date[1:33] , col="grey")
d_hr_gs_2 <- merge(d_hr_gs, d_mei , by="year")
d_hr_gs_2 <- d_hr_gs_2[d_hr_gs_2$month=="JJ",]
min(d_mei$year)
d_mei$year_index_overall <- d_mei$year - 1989


#all groups
plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" , cex=0.7 , ylim=c(-2.5,2.5))
lines(mei_spl, col = "grey3")
points( d_hr_gs_2$date , standardize(d_hr_gs_2$hr_area_mean) , col=group_pal[d_hr_gs_2$group_index] , pch=19)
abline(v=d_mei$date[1:33] , col="grey")
for(i in c(1:3,5:11)){
  grp_spl <- with(d_hr_gs_2[d_hr_gs_2$group_index==i,], smooth.spline(date, mei ,spar=.5))
  lines(grp_spl, col = group_pal[i])
}


###mei consolidate
str(d_hr_gs_2)
mean_df <- aggregate(mei ~ year, d_mei, mean)
names(mean_df)[2] <- "mean_annual_mei"
max_df <- aggregate(mei ~ year, d_mei, max)
names(max_df)[2] <- "max_annual_mei"
min_df <- aggregate(mei ~ year, d_mei, min)
names(min_df)[2] <- "min_annual_mei"
sd_df <- aggregate(mei ~ year, d_mei, sd)
names(sd_df)[2] <- "sd_annual_mei"
## get akdes
# get UD telemetry object
UD <- readRDS("~/Downloads/slp_1990-2019_RSF_AKDEs.rds")

# function to get summary information from AKDEs
summarize_akde <- function(akde){
  
  summary <- summary(akde, units = FALSE) # makes the units fro all UDs the same (m2)
  
  tibble(id = akde@info$identity, 
         DOF = summary$DOF[1],
         low = (summary$CI[1])/1000000, # convert m2 to km2
         area = (summary$CI[2])/1000000,
         high = (summary$CI[3])/1000000)
}

# wrapper to stack area info into data frame
make_df <- function(id){
  map_dfr(id, summarize_akde) 
}

# apply functions to get data frame and calculate shape and rate
d_akde <- make_df(UD) %>% 
  mutate(scale = area/DOF,
         rate=DOF/area , 
         shape = DOF)
str(d_akde)
##compile bigger data frames

d_hr_gs_3 <- merge(d_hr_gs, mean_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, min_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, max_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, sd_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, d_akde , by="id")

d_hr_gs_3$year_index <- as.integer(as.factor(d_hr_gs_3$year))

d_mei <- d_mei %>%
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  arrange(date)
d_mei$year_analyze <- NA

min(d_mei$date[d_mei$year==1991])
#make mei datasets
pasta <- min(which(d_mei$year==1991))
d_mei[pasta:(pasta+11),]

#add wet and dry season to mei, 1st 4 months of year is dry
d_mei$season <- ifelse( month(d_mei$date) < 5 , "dry" , "wet" )
d_mei$season_index <- as.integer(as.factor(d_mei$season))
###for same year
d_mei_hr_data <- d_mei[is.element(d_mei$year , d_hr_gs_3$year),]


list_area <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index
)


drip <- read.csv("data/df_annual_riparian.csv")
drip <- drip[drip$year < 2020,]
str(drip)
drip$group_index <- as.integer(as.factor(drip$group))
# drip$group_size_std <- standardize(drip$group_size)

d_mei_hr_data$year_index_mei <- as.integer(as.factor(d_mei_hr_data$year))
drip$year_index <- as.integer(as.factor(drip$year))

# drip$mei_sample_mean <- 0
# for(i in 1:nrow(drip)){
#   drip$mei_sample_mean[i] <- mean(d_mei_hr_data$mei[d_mei_hr_data$season==drip$season[i] & d_mei_hr_data$year==drip$year[i]])
# }
d_mei_hr_data_2 <- d_mei[is.element(d_mei$year , drip$year),]

str(d_mei_hr_data_2)
d_mei_hr_data_2 <- d_mei_hr_data_2[d_mei_hr_data_2$year < 2020,]
drip <- merge(drip,d_hr_gs[,c(4,5,11)],by="id") 

##needs to be updated for 2023
list_rip <- list(
  hr_area=round(drip$hr_area),
  intersect_area=round(drip$intersect_area) ,
  prop_river=drip$prop_river ,
  group_index=drip$group_index ,
  # group_size=drip$group_size ,
  year_index=as.integer(as.factor(drip$year)),
  year=as.integer(drip$year),
  mei=d_mei_hr_data_2$mei,
  year_index_mei=as.integer(as.factor(d_mei_hr_data_2$year)),
  N_years=length(unique(d_mei_hr_data_2$year)),
  N=nrow(drip) ,
  N_groups=length(unique(drip$group_index)),
  group_size_std=drip$group_size_std ,
  group_size=drip$group_size
)


