library(rsoi)
library(janitor)
library(RColorBrewer)
library(lubridate)
library(ctmm)
library(tidyverse)
library(rethinking)

#get enso data
mei <- clean_names(download_mei())
d_mei <- mei[mei$year >= min(d_hr_gs$year),]
d_mei <- d_mei[complete.cases(d_mei),]
plot(d_mei$mei~d_mei$date)

#combie data frames, will do posterior across time series later
str(d_hr_gs)
str(d_mei)
elcol_pal <- rev(brewer.pal(3 , "RdYlBu"))
group_pal <- brewer.pal(11 , "Spectral")

d_mei$phase_index <- as.integer(d_mei$phase)
plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch=19 , cex=0. , xlab="year" , ylab="MEI index")
mei_spl <- with(d_mei, smooth.spline(date, mei))
lines(mei_spl, col = "grey3")
abline(v=d_mei$date[1:33] , col="grey")
d_hr_gs_2 <- merge(d_hr_gs, d_mei , by="year")
d_hr_gs_2 <- d_hr_gs_2[d_hr_gs_2$month=="JJ",]
min(d_mei$year)
d_mei$year_index_overall <- d_mei$year - 1990

#all groups
# plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" , cex=0.7 , ylim=c(-2.5,2.5))
# lines(mei_spl, col = "grey3")
# #points( d_hr_gs_2$date , standardize(d_hr_gs_2$hr_area_mean) , col=group_pal[d_hr_gs_2$group_index] , pch=19)
# abline(v=d_mei$date[1:33] , col="grey")
# for(i in c(1:3,5:11)){
#   grp_spl <- with(d_hr_gs_2[d_hr_gs_2$group_index==i,], smooth.spline(date, mei ,spar=.5))
#   lines(grp_spl, col = group_pal[i])
# }

#per group plot
# for(i in 1:11){
#   plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" ,
#        cex=0.7 , ylim=c(-2.5,2.5) , main=min(d_hr_gs_2$group[d_hr_gs_2$group_index==i] ) )
#   lines(mei_spl, col = "grey3")
#   points( d_hr_gs_2$date[d_hr_gs_2$group_index==i] , 
#           standardize(d_hr_gs_2$hr_area_mean[d_hr_gs_2$group_index==i]) , 
#           col=group_pal[i] , pch=19)
#   abline(v=d_mei$date[1:33] , col="grey")
# }

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

### READ IN AREA DF
d_hr_gs <- read.csv("data/df_slpHRarea_group_size.csv")

##compile bigger data frames

d_hr_gs_3 <- merge(d_hr_gs, mean_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, min_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, max_df , by="year")
d_hr_gs_3 <- merge(d_hr_gs_3, sd_df , by="year")
#d_hr_gs_3 <- merge(d_hr_gs_3, d_akde , by="id")

d_hr_gs_3$year_index <- as.integer(as.factor(d_hr_gs_3$year))
d_mei_hr_data <- d_mei[is.element(d_mei$year , d_hr_gs_3$year),]

str(d_hr_gs_3)

# add columns
d_hr_gs_3$hr_area_mean <- d_hr_gs_3$area
d_hr_gs_3$hr_area_low <- d_hr_gs_3$low
d_hr_gs_3$hr_area_high <- d_hr_gs_3$high
d_hr_gs_3$group_index <- as.integer(as.factor(d_hr_gs_3$group))
d_hr_gs_3$group_size_std <- standardize(d_hr_gs_3$group_size)


list_area <- list(
  hr_area_mean=d_hr_gs_3$hr_area_mean ,
  hr_area_high=d_hr_gs_3$hr_area_high ,
  hr_area_low=d_hr_gs_3$hr_area_low ,
  #hr_area_sd=d_hr_gs_3$hr_area_sd ,
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
  #hr_area_sd=d_hr_gs_3$hr_area_sd ,
  hr_area_rate=d_hr_gs_3$rate ,
  hr_area_shape=d_hr_gs_3$shape ,
  group_index=d_hr_gs_3$group_index ,
  group_size=d_hr_gs_3$group_size ,
  group_size_std=d_hr_gs_3$group_size_std ,
  log_group_size=log(d_hr_gs_3$group_size) ,
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
  kde_scale=d_hr_gs_3$scale ,
  phase_index=d_mei_hr_data$phase_index,
  N_mei = length(d_mei_hr_data$mei)
)

###visually inspect rate shape
library(rethinking)
library('dagitty')
set_ulam_cmdstan(TRUE)
###visually inspect eate shape

for(i in 10:20){
  plot(density(rgamma(10000,shape=d_hr_gs_3$shape[[i]], rate=d_hr_gs_3$rate[[i]] ) , xlim=c(0,10)) , main="blah" )
  lines(density(rgamma(10000,shape=d_hr_gs_3$shape[[i]], scale=d_hr_gs_3$scale[[i]] ) ) , lty=2 )
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

##below is model in supplemental actually used in paper

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
post_hrgsmei <- extract.samples(fit_hr_mei_meas_er)
#precis(fit_hr, depth=2 , pars=c("sigma_g"))

###below is model used in main result
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
precis(fit_hr_mei_gs_meas_er , depth=2 , c("v_mu" ))
precis(fit_hr_mei_gs_meas_er , depth=2 , c("v_mu" , "sigma_g"))
precis(fit_hr_mei_gs_meas_er , depth=3, c("Rho_g") )

###home range only
file_name <- 'stan_code/hr_gs_meas_er.stan'
fit_hr_gs_meas_er= stan( file = file_name,
                             data = list_area_2 ,
                             iter = 4000,
                             chains=4,
                             cores=4,
                             control=list(adapt_delta=0.99) ,
                             refresh=250,
                             init=0,
                             seed=169
)

precis(fit_hr_gs_meas_er, depth=2 , pars=c("v_mu" , "sigma_g" , "k") )

post_hrgs <- extract.samples(fit_hr_gs_meas_er)

## home range on log scale, is same as if you don't log it does not matter
list_area_2_log <- list_area_2
list_area_2_log$group_size <-  list_area_2_log$log_group_size
file_name <- 'stan_code/hr_gs_meas_er.stan'
fit_hr_loggs_meas_er= stan( file = file_name,
                         data = list_area_2_log ,
                         iter = 4000,
                         chains=4,
                         cores=4,
                         control=list(adapt_delta=0.99) ,
                         refresh=250,
                         init=0,
                         seed=169
)

precis(fit_hr_loggs_meas_er, depth=2 , pars=c("v_mu" , "sigma_g" , "k") )

post_hrgs <- extract.samples(fit_hr_gs_meas_er)


file_name <- 'stan_code/rip_mei_meas_er.stan' #stupid name
fit_mei_rip = stan( file = file_name,
                   data = list_rip ,
                   iter = 3000,
                   chains=4,
                   cores=4,
                   control=list(adapt_delta=0.99) ,
                   refresh=250,
                   seed=1239
)

post_meirip <- extract.samples(fit_mei_rip)
precis(fit_mei_rip , depth=2)
precis(fit_mei_rip , depth=3 , pars="v")
precis(fit_mei_rip , depth=3 , pars="Rho_g")
