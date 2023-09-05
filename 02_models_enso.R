library(rsoi)
library(janitor)
library(RColorBrewer)
library(lubridate)
library(ctmm)
library(tidyverse)

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

#per group plot
for(i in 1:11){
  plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" ,
       cex=0.7 , ylim=c(-2.5,2.5) , main=min(d_hr_gs_2$group[d_hr_gs_2$group_index==i] ) )
  lines(mei_spl, col = "grey3")
  points( d_hr_gs_2$date[d_hr_gs_2$group_index==i] , 
          standardize(d_hr_gs_2$hr_area_mean[d_hr_gs_2$group_index==i]) , 
          col=group_pal[i] , pch=19)
  abline(v=d_mei$date[1:33] , col="grey")
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

#one year, shifted 6 months
pasta <- min(which(d_mei$year==1991))
mei_12m_6shift <- d_mei[(pasta-6):(pasta+5),]
mei_12m_6shift$year_index_overall <- 1
mei_12m_6shift$year_analyze <- 1991
#fuck with indexing
for(swag in 1992:max(d_mei$year)){
  shins <- min(which(d_mei$year==swag))
  mei_12m_6shift <- rbind(mei_12m_6shift, d_mei[(shins-6):(shins+5),] )
  meow <- nrow(mei_12m_6shift)
  mei_12m_6shift$year_index_overall[(meow-11):meow] <- swag-1990
  mei_12m_6shift$year_analyze[(meow-11):meow]  <- rep(swag,12)
}
mei_12m_6shift

#one year, shifted 12 months
pasta <- min(which(d_mei$year==1991))
mei_12m_12shift <- d_mei[(pasta-11):(pasta-1),]
mei_12m_12shift$year_index_overall <- 1
mei_12m_12shift$year_analyze <- 1991
#fuck with indexing
for(swag in 1992:max(d_mei$year)){
  shins <- min(which(d_mei$year==swag))
  mei_12m_12shift <- rbind(mei_12m_12shift, d_mei[(shins-11):(shins-1),] )
  meow <- nrow(mei_12m_12shift)
  mei_12m_12shift$year_index_overall[(meow-11):meow] <- swag-1990
  mei_12m_12shift$year_analyze[(meow-11):meow]  <- rep(swag,12)
}
mei_12m_12shift

# we need timescales
#same year
#year shifted six months
#two years

# d_hr_ov$year <- d_hr_ov$y1
# d_hr_ov_3 <-merge(d_hr_ov, mean_df , by="year")
###for same year
d_mei_hr_data <- d_mei[is.element(d_mei$year , d_hr_gs_3$year),]
#for 1 year 6 mos shifted
d_mei_hr_data_6mosshift <- mei_12m_6shift[is.element(mei_12m_6shift$year_analyze , d_hr_gs_3$year),]
d_mei_hr_data_12mosshift <- mei_12m_12shift[is.element(mei_12m_12shift$year_analyze , d_hr_gs_3$year),]



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
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index,
  mei=d_mei_hr_data$mei ,
  year_mei=d_mei_hr_data$year ,
  year_index_mei=as.integer(as.factor(d_mei_hr_data$year)),
  N_years=length(unique(d_mei_hr_data$year)),
  N=nrow(d_hr_gs_3) ,
  N_groups=length(unique(d_hr_gs_3$group_index)) ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
  kde_shape=d_hr_gs_3$shape ,
  kde_rate=d_hr_gs_3$rate ,
  kde_scale=d_hr_gs_3$scale 
)

list_area_3 <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  hr_area_rate=d_hr_gs_3$rate ,
  hr_area_shape=d_hr_gs_3$shape ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index,
  mei=d_mei_hr_data_6mosshift$mei ,
  year_mei=d_mei_hr_data_6mosshift$year_analyze ,
  year_index_mei=as.integer(as.factor(d_mei_hr_data_6mosshift$year_analyze)) ,
  N_years=length(unique(d_mei_hr_data_6mosshift$year_analyze)) ,
  N=nrow(d_hr_gs_3) ,
  N_groups=length(unique(d_hr_gs_3$group_index)) ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
  kde_shape=d_hr_gs_3$shape ,
  kde_rate=d_hr_gs_3$rate ,
  kde_scale=d_hr_gs_3$scale 
)

list_area_4 <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  hr_area_sd=d_hr_gs_3$hr_area_sd ,
  hr_area_rate=d_hr_gs_3$rate ,
  hr_area_shape=d_hr_gs_3$shape ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size_std ,
  year_index=d_hr_gs_3$year_index,
  mei=d_mei_hr_data_12mosshift$mei ,
  year_mei=d_mei_hr_data_12mosshift$year_analyze ,
  year_index_mei=as.integer(as.factor(d_mei_hr_data_12mosshift$year_analyze)) ,
  N_years=length(unique(d_mei_hr_data_12mosshift$year_analyze)) ,
  N=nrow(d_hr_gs_3) ,
  N_groups=length(unique(d_hr_gs_3$group_index)) ,
  mean_annual_mei=d_hr_gs_3$mean_annual_mei ,
  min_annual_mei=d_hr_gs_3$min_annual_mei ,
  max_annual_mei=d_hr_gs_3$max_annual_mei ,
  sd_annual_mei=d_hr_gs_3$sd_annual_mei ,
  kde_shape=d_hr_gs_3$shape ,
  kde_rate=d_hr_gs_3$rate ,
  kde_scale=d_hr_gs_3$scale 
)



min(d_mei_hr_data)
which(d_mei_hr_data$mei)
mei_same_year
mei_year_six_shift
###visually inspect eate shape
for(i in 10:20){
  plot(density(rgamma(10000,shape=d_akde$shape[[i]], rate=d_akde$rate[[i]] ) , xlim=c(0,10)) , main="blah" )
  lines(density(rgamma(10000,shape=d_akde$shape[[i]], scale=d_akde$scale[[i]] ) ) , lty=2 )
  points( d_akde$area[i] , 0.1 )
  segments(  x0=d_akde$low[i], y0=0.1 , x1=d_akde$high[i] ,y1= 0.1 , col="blue")
}


##stan models
file_name <- 'stan_code/test_mei.stan'
fit= stan( file = file_name,
              data = list_area_2 ,
              iter = 1000,
              chains=4,
              cores=4,
              control=list(adapt_delta=0.9) ,
              refresh=100,
              init=0,
              seed=12
)

precis(fit , depth=2)
post <- extract.samples(fit)
for(i in 1:24){
  dens(post$am[,i] , xlim=c(-3,3))
  dens(post$am_pred[,i], add=TRUE , lty=2)
  points(list_area_2$mei[list_area_2$year_index_mei==i] , rep(0,12), col="red")
}

file_name <- 'stan_code/mei_hr.stan'
fit_hr= stan( file = file_name,
            data = list_area_2 ,
            iter = 4000,
            chains=4,
            cores=4,
            control=list(adapt_delta=0.99) ,
            refresh=250,
            init=0,
            seed=232
)

precis(fit_hr , depth=2)

file_name <- 'stan_code/mei_hr_gs.stan'
fit_hr_gs= stan( file = file_name,
              data = list_area_2 ,
              iter = 4000,
              chains=4,
              cores=4,
              control=list(adapt_delta=0.99) ,
              refresh=250,
              init=0,
              seed=813
)

precis(fit_hr_gs , depth=2)

#####hr shape scale

file_name <- 'stan_code/hr_meas.stan'
fit_hr_post= stan( file = file_name,
                 data = list_area_2 ,
                 iter = 4000,
                 chains=4,
                 cores=4,
                 control=list(adapt_delta=0.99) ,
                 refresh=250,
                 init=0,
                 seed=813
)
precis(fit_hr_post , depth=2)
par(mfrow=c(5,6), oma=c(0,0,0,0), mar=c(0,0,0,0) )
post <- extract.samples(fit_hr_post)
for (i in 1:130){
  dens(post$hr_area_true[,i] , xlim=c(0,6) ,col="red")
  dens(rgamma(2000,shape=d_akde$shape[[i]], rate=d_akde$rate[[i]] ) , add=TRUE )
  lines(density(rgamma(2000,shape=d_akde$shape[[i]], scale=d_akde$scale[[i]] ) ) , lty=2 )
  points( d_akde$area[i] , 0.1 )
  segments(  x0=d_akde$low[i], y0=0.1 , x1=d_akde$high[i] ,y1= 0.1 , col="blue")
}

file_name <- 'stan_code/hr_meas_varef.stan'
fit_hr_varef= stan( file = file_name,
              data = list_area_2 ,
              iter = 4000,
              chains=4,
              cores=4,
              control=list(adapt_delta=0.99) ,
              refresh=250,
              init=0,
              seed=813
)
precis(fit_hr_varef , depth=2)


file_name <- 'stan_code/hr_mei_meas_er.stan'
fit_hr_mei_meas_er= stan( file = file_name,
                    data = list_area_2 ,
                    iter = 4000,
                    chains=4,
                    cores=4,
                    control=list(adapt_delta=0.99) ,
                    refresh=250,
                    init=0,
                    seed=3169
)
precis(fit_hr_mei_meas_er , depth=2 , c("sigma_g"))
#precis(fit_hr, depth=2 , pars=c("sigma_g"))

file_name <- 'stan_code/hr_mei_gs_meas_er.stan'
fit_hr_mei_gs_meas_er= stan( file = file_name,
                          data = list_area_2 ,
                          iter = 4000,
                          chains=4,
                          cores=4,
                          control=list(adapt_delta=0.99) ,
                          refresh=250,
                          init=0,
                          seed=169
)
precis(fit_hr_mei_gs_meas_er , depth=2 , c("v_mu" , "sigma_g"))
precis(fit_hr_mei_gs_meas_er , depth=2 , c("v_mu" , "sigma_g"))
precis(fit_hr_mei_gs_meas_er , depth=3 )

#6 mos shift
file_name <- 'stan_code/hr_mei_meas_er.stan'
fit_hr_mei_meas_er_6mosshift= stan( file = file_name,
                          data = list_area_3 ,
                          iter = 4000,
                          chains=4,
                          cores=4,
                          control=list(adapt_delta=0.99) ,
                          refresh=250,
                          init=0,
                          seed=3169
)
precis(fit_hr_mei_meas_er_6mosshift , depth=2 , c("sigma_g"))
precis(fit_hr_mei_meas_er_6mosshift , depth=2 , c("v_mu","sigma_g"))

file_name <- 'stan_code/hr_mei_gs_meas_er.stan'
fit_hr_mei_gs_meas_er_6mosshift= stan( file = file_name,
                             data = list_area_3 ,
                             iter = 4000,
                             chains=4,
                             cores=4,
                             control=list(adapt_delta=0.99) ,
                             refresh=250,
                             init=0,
                             seed=169
)
precis(fit_hr_mei_gs_meas_er_6mosshift , depth=2 , c("v_mu" , "sigma_g"))
precis(fit_hr_mei_gs_meas_er_6mosshift , depth=3 )



#####seasonal models
d_hr_seas <-  read.csv("data/df_slpHRarea_seasonal_error.csv")
str(d_hr_seas)
d_hr_seas$group <- substr(d_hr_seas$id, 1, 2)
d_hr_seas$group_index <- as.integer(as.factor(d_hr_seas$group))
d_hr_seas$group_size_std <- standardize(d_hr_seas$group_size)
d_hr_seas$seas <- substr(d_hr_seas$id, 4, 6)
d_hr_seas$seas_index <- as.integer(as.factor(d_hr_seas$seas))
d_hr_seas$year <-substr(d_hr_seas$id, start=8 , stop=16)
