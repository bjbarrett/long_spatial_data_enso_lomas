library(rethinking)
library(janitor)
library(dplyr)
library(tidyverse)

d_hr_gs_seas <- read.csv("data/df_slpHRarea_seasonal.csv")
str(d_hr_gs_seas)
unique(d_hr_gs_seas)
d_hr_gs_seas$group <- substr(d_hr_gs_seas$id, 1, 2)
d_hr_gs_seas$group_index <- as.integer(as.factor(d_hr_gs_seas$group))
d_hr_gs_seas$group_size_std <- standardize(d_hr_gs_seas$group_size)
d_hr_gs_seas$season <- substr(d_hr_gs_seas$id, 4, 6)
d_hr_gs_seas$season_index <- as.integer(as.factor(d_hr_gs_seas$season))

#add years

mei_seas <- clean_names(download_mei())
d_mei_seas <- mei_seas[mei_seas$year >= min(d_hr_gs_seas$year),]
d_mei_seas <- d_mei_seas[complete.cases(d_mei_seas),]
