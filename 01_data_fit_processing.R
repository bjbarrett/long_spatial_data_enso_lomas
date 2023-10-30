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

pdf(file="plots/all_da_groups_raw.pdf" , width = 8 , height=11)
par(mfrow = c(11, 1))
par(mar = rep(1,4) +0.1, oma = rep(0,4) +0.1)
#per group plot
for(i in 1:11){
  plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" ,
       cex=0.7 , ylim=c(-2.5,2.5) , main=min(d_hr_gs_2$group[d_hr_gs_2$group_index==i] ) )
  #lines(mei_spl, col = "grey3")
  points( d_hr_gs_2$date[d_hr_gs_2$group_index==i] , 
          standardize(d_hr_gs_2$hr_area_mean[d_hr_gs_2$group_index==i]) , 
          col=group_pal[i] , pch=19)
  abline(v=d_mei$date[i+i*11] , col="grey")
  for (i in 1:33) abline(v=d_mei$date[i+i*11] , col="grey")
  
}
dev.off()

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
UD <- readRDS("/Users/sifaka/Downloads/slp_1990-2019_RSF_AKDEs.rds")

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
mei_12m_same <- d_mei[pasta:(pasta+11),]
for(swag in 1992:max(d_mei$year)){
  shins <- min(which(d_mei$year==swag))
  mei_12m_same <- rbind(mei_12m_same , d_mei[(shins-6):(shins+5),] )
}
mei_12m_same
# 
# #one year, shifted 6 months
# pasta <- min(which(d_mei$year==1991))
# mei_12m_6shift <- d_mei[(pasta-6):(pasta+5),]
# mei_12m_6shift$year_index_overall <- 1
# mei_12m_6shift$year_analyze <- 1991
# #fuck with indexing
# for(swag in 1992:max(d_mei$year)){
#   shins <- min(which(d_mei$year==swag))
#   mei_12m_6shift <- rbind(mei_12m_6shift, d_mei[(shins-6):(shins+5),] )
#   meow <- nrow(mei_12m_6shift)
#   mei_12m_6shift$year_index_overall[(meow-11):meow] <- swag-1990
#   mei_12m_6shift$year_analyze[(meow-11):meow]  <- rep(swag,12)
# }
# mei_12m_6shift
# 
# #one year, shifted 12 months
# pasta <- min(which(d_mei$year==1991))
# mei_12m_12shift <- d_mei[(pasta-11):(pasta-1),]
# mei_12m_12shift$year_index_overall <- 1
# mei_12m_12shift$year_analyze <- 1991
# #fuck with indexing
# for(swag in 1992:max(d_mei$year)){
#   shins <- min(which(d_mei$year==swag))
#   mei_12m_12shift <- rbind(mei_12m_12shift, d_mei[(shins-11):(shins-1),] )
#   meow <- nrow(mei_12m_12shift)
#   mei_12m_12shift$year_index_overall[(meow-11):meow] <- swag-1990
#   mei_12m_12shift$year_analyze[(meow-11):meow]  <- rep(swag,12)
# }
# mei_12m_12shift

# we need timescales
#same year
#year shifted six months
#two years

# d_hr_ov$year <- d_hr_ov$y1
# d_hr_ov_3 <-merge(d_hr_ov, mean_df , by="year")
#add wet and dry season to mei, 1st 4 months of year is dry
d_mei$season <- ifelse( month(d_mei$date) < 5 , "dry" , "wet" )
d_mei$season_index <- as.integer(as.factor(d_mei$season))
###for same year
d_mei_hr_data <- d_mei[is.element(d_mei$year , d_hr_gs_3$year),]


#for 1 year 6 mos shifted
# d_mei_hr_data_6mosshift <- mei_12m_6shift[is.element(mei_12m_6shift$year_analyze , d_hr_gs_3$year),]
# d_mei_hr_data_12mosshift <- mei_12m_12shift[is.element(mei_12m_12shift$year_analyze , d_hr_gs_3$year),]




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

list_area_2 <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  hr_area_rate=d_hr_gs_3$rate ,
  hr_area_shape=d_hr_gs_3$shape ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size ,
  group_size_std=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index,
  mei=d_mei_hr_data$mei ,
  year_mei=d_mei_hr_data$year ,
  year_index_mei=as.integer(as.factor(d_mei_hr_data$year)),
  phase_index=d_mei_hr_data$phase_index,
  N_years=length(unique(d_mei_hr_data$year)),
  N=nrow(d_hr_gs_3) ,
  N_groups=length(unique(d_hr_gs_3$group_index)) ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
  kde_shape=d_hr_gs_3$shape ,
  kde_rate=d_hr_gs_3$rate ,
  kde_scale=d_hr_gs_3$scale ,
  N_mei = nrow(d_mei_hr_data)
  
)

# list_area_3 <- list(
#   hr_area_mean=d_hr_gs_3$hr_area_mean ,
#   hr_area_high=d_hr_gs_3$hr_area_high ,
#   hr_area_low=d_hr_gs_3$hr_area_low ,
#   hr_area_sd=d_hr_gs_3$hr_area_sd ,
#   hr_area_rate=d_hr_gs_3$rate ,
#   hr_area_shape=d_hr_gs_3$shape ,
#   group_index=d_hr_gs_3$group_index ,
#   group_size=d_hr_gs_3$group_size_std ,
#   year_index=d_hr_gs_3$year_index,
#   mei=d_mei_hr_data_6mosshift$mei ,
#   year_mei=d_mei_hr_data_6mosshift$year_analyze ,
#   year_index_mei=as.integer(as.factor(d_mei_hr_data_6mosshift$year_analyze)) ,
#   N_years=length(unique(d_mei_hr_data_6mosshift$year_analyze)) ,
#   N=nrow(d_hr_gs_3) ,
#   N_groups=length(unique(d_hr_gs_3$group_index)) ,
#   mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
#   min_annual_mei=d_hr_gs_3$min_annual_mei ,
#   max_annual_mei=d_hr_gs_3$max_annual_mei ,
#   sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
#   kde_shape=d_hr_gs_3$shape ,
#   kde_rate=d_hr_gs_3$rate ,
#   kde_scale=d_hr_gs_3$scale 
# )
# 
# list_area_4 <- list(
#   hr_area_mean=d_hr_gs_3$hr_area_mean ,
#   hr_area_high=d_hr_gs_3$hr_area_high ,
#   hr_area_low=d_hr_gs_3$hr_area_low ,
#   hr_area_sd=d_hr_gs_3$hr_area_sd ,
#   hr_area_rate=d_hr_gs_3$rate ,
#   hr_area_shape=d_hr_gs_3$shape ,
#   group_index=d_hr_gs_3$group_index ,
#   group_size=d_hr_gs_3$group_size_std ,
#   year_index=d_hr_gs_3$year_index,
#   mei=d_mei_hr_data_12mosshift$mei ,
#   year_mei=d_mei_hr_data_12mosshift$year_analyze ,
#   year_index_mei=as.integer(as.factor(d_mei_hr_data_12mosshift$year_analyze)) ,
#   N_years=length(unique(d_mei_hr_data_12mosshift$year_analyze)) ,
#   N=nrow(d_hr_gs_3) ,
#   N_groups=length(unique(d_hr_gs_3$group_index)) ,
#   mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
#   min_annual_mei=d_hr_gs_3$min_annual_mei ,
#   max_annual_mei=d_hr_gs_3$max_annual_mei ,
#   sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
#   kde_shape=d_hr_gs_3$shape ,
#   kde_rate=d_hr_gs_3$rate ,
#   kde_scale=d_hr_gs_3$scale 
# )

#########READ IN SEASONAL DATA FRAME

##extract important things from strings
d_hr_seas <- read.csv("data/df_slpHRarea_seasonal_jan-apr_RSF_error.csv")
d_hr_seas$group <- substr(d_hr_seas$id, start = 1, stop = 2)
d_hr_seas$season <- substr(d_hr_seas$id, start = 4, stop = 6)
d_hr_seas$year <- substr(d_hr_seas$id, start = 8, stop = 11)
#integer dat jawn
d_hr_seas$group_index <- as.integer(as.factor(d_hr_seas$group))
d_hr_seas$season_index <- as.integer(as.factor(d_hr_seas$season))
d_hr_seas$year_index <- as.integer(as.factor(d_hr_seas$year))

str(d_hr_seas)
#extract group
d_hr_seas$group_index <- as.integer(as.factor(d_hr_seas$group))
d_hr_seas$group_size_std <- standardize(d_hr_seas$group_size)
#shape and rate
d_hr_seas$scale <- d_hr_seas$mean_area/d_hr_seas$DOF
d_hr_seas$rate <- d_hr_seas$DOF/d_hr_seas$mean_area
d_hr_seas$shape <- d_hr_seas$DOF

d_mei_hr_data$year_index_mei <- as.integer(as.factor(d_mei_hr_data$year))

list_area_seas <- list(
  hr_area_mean=d_hr_seas$mean_area ,
  group_index=d_hr_seas$group_index ,
  group_size=d_hr_seas$group_size ,
  year_index=d_hr_seas$year_index,
  year=as.integer(d_hr_seas$year),
  # mei=d_mei_hr_data$mei ,
  # year_mei=d_mei_hr_data$year ,
  # year_index_mei=as.integer(as.factor(d_mei_hr_data$year)),
  # N_years=length(unique(d_mei_hr_data$year)),
  mei_dry=d_mei_hr_data$mei[d_mei_hr_data$season_index==1] ,
  mei_wet=d_mei_hr_data$mei[d_mei_hr_data$season_index==2] ,
  #year_mei=d_mei_hr_data$year ,
  year_index_mei_dry=d_mei_hr_data$year_index_mei[d_mei_hr_data$season_index==1],
  year_index_mei_wet=d_mei_hr_data$year_index_mei[d_mei_hr_data$season_index==2],
  N_years=length(unique(d_mei_hr_data$year)),
  #season_index_mei=d_mei_hr_data$season_index ,
  season_index=d_hr_seas$season_index ,
  N=nrow(d_hr_seas) ,
  N_groups=length(unique(d_hr_seas$group_index)) ,
  kde_shape=d_hr_seas$shape ,
  kde_rate=d_hr_seas$rate ,
  kde_scale=d_hr_seas$scale,
  wet=d_hr_seas$season_index - 1,
  group_size_std=d_hr_seas$group_size_std
)
str(list_area_seas)


###overlap
d_ov <- read.csv("data/df_slpHR_dyadic_overlap.csv")
str(d_ov)
d_ov$year1 <- as.integer(str_sub(d_ov$p1,4,7))
d_ov$year2 <- as.integer(str_sub(d_ov$p2,4,7))
d_ov$group1 <- str_sub(d_ov$p1,1,2)
d_ov$group2 <- str_sub(d_ov$p2,1,2)

d_ov2 <- d_ov[which(d_ov$year1==d_ov$year2),]
d_ov2 <- d_ov2[which(d_ov2$group1!=d_ov2$group2),]
d_ov2 <- d_ov2[d_ov2$overlap_uds>0,] #drop zeros for now
sort(unique(d_ov2$group1))==sort(unique(d_ov2$group2))##if true we good for model
sort(unique(d_ov2$group1))
sort(unique(d_ov2$group2))
#get sp into g1 and aa into g2
criteria <- which(d_ov2$group1=="aa" & d_ov2$group2=="sp")
d_ov2$group1[criteria[1:2]] <- "sp"
d_ov2$group2[criteria[1:2]] <- "aa"
sort(unique(d_ov2$group1))==sort(unique(d_ov2$group2))##if true we good for model
sort(unique(d_ov2$group1))
sort(unique(d_ov2$group2))

d_ov2$g1_index <- as.integer(as.factor(d_ov2$group1))
d_ov2$g2_index <- as.integer(as.factor(d_ov2$group2))
d_ov2$year_index <- as.integer(as.factor(d_ov2$year1))
sort(unique(d_ov2$year1))==sort(unique(d_ov2$year2)) #should be true too
d_ov2$dyad <- apply(d_ov2[,7:8], 1, function(s) paste0(sort(s), collapse='')) #get dyad level indicators
d_ov2$d_index <- as.integer(as.factor(d_ov2$dyad))
d_ov2$y


d_mei_ov_data <- d_mei_hr_data[which(d_mei_hr_data$year %in% unique(d_ov2$year1)),]
d_mei_ov_data$year_index_mei <- as.integer(as.factor(d_mei_ov_data$year))
#note that a zero augmented beta is the way to go, but i will drop zeros and do beta for now
list_ov <- list(
  N=nrow(d_ov2) ,
  N_groups=length(unique(d_ov2$g1_index)) ,
  N_dyads=length(unique(d_ov2$d_index)) ,
  overlap_uds = d_ov2$overlap_uds ,
  g1_index=d_ov2$g1_index ,
  g2_index=d_ov2$g2_index ,
  d_index=d_ov2$d_index,
  year_index=d_ov2$year_index,
  year=d_ov2$year1,
  mei=d_mei_ov_data$mei ,
  year_mei=d_mei_ov_data$year ,
  year_index_mei=d_mei_ov_data$year_index_mei,
  N_years=length(unique(d_mei_ov_data$year)) ,
  N_mei = nrow(d_mei_ov_data)
)


### riparian data
drip <- read.csv("data/df_seasonal_riparian.csv")
str(drip)
drip$group_index <- as.integer(as.factor(drip$group))
drip$group_size_std <- standardize(drip$group_size)

d_mei_hr_data$year_index_mei <- as.integer(as.factor(d_mei_hr_data$year))
drip$season_index <- as.integer(as.factor(drip$season))
drip$year_index <- as.integer(as.factor(drip$year))

drip$mei_sample_mean <- 0
for(i in 1:nrow(drip)){
 drip$mei_sample_mean[i] <- mean(d_mei_hr_data$mei[d_mei_hr_data$season==drip$season[i] & d_mei_hr_data$year==drip$year[i]])
}

list_rip <- list(
  hr_area=round(drip$hr_area),
  intersect_area=round(drip$intersect_area) ,
  prop_river=drip$prop_river ,
  group_index=drip$group_index ,
  group_size=drip$group_size ,
  year_index=drip$year_index,
  year=as.integer(drip$year),
  mei_dry=d_mei_hr_data$mei[d_mei_hr_data$season_index==1] ,
  mei_wet=d_mei_hr_data$mei[d_mei_hr_data$season_index==2] ,
  year_index_mei_dry=d_mei_hr_data$year_index_mei[d_mei_hr_data$season_index==1],
  year_index_mei_wet=d_mei_hr_data$year_index_mei[d_mei_hr_data$season_index==2],
  N_years=length(unique(d_mei_hr_data$year)),
  season_index=drip$season_index ,
  N=nrow(drip) ,
  N_groups=length(unique(drip$group_index)) ,
  wet=drip$season_index - 1,
  group_size_std=drip$group_size_std

)
