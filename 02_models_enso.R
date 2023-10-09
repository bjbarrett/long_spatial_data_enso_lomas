library(rsoi)
library(janitor)
library(RColorBrewer)
library(lubridate)
library(ctmm)
library(tidyverse)




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

# #6 mos shift
# file_name <- 'stan_code/hr_mei_meas_er.stan'
# fit_hr_mei_meas_er_6mosshift= stan( file = file_name,
#                           data = list_area_3 ,
#                           iter = 4000,
#                           chains=4,
#                           cores=4,
#                           control=list(adapt_delta=0.99) ,
#                           refresh=250,
#                           init=0,
#                           seed=3169
# )
# precis(fit_hr_mei_meas_er_6mosshift , depth=2 , c("sigma_g"))
# precis(fit_hr_mei_meas_er_6mosshift , depth=2 , c("v_mu","sigma_g"))
# 
# file_name <- 'stan_code/hr_mei_gs_meas_er.stan'
# fit_hr_mei_gs_meas_er_6mosshift= stan( file = file_name,
#                              data = list_area_3 ,
#                              iter = 4000,
#                              chains=4,
#                              cores=4,
#                              control=list(adapt_delta=0.99) ,
#                              refresh=250,
#                              init=0,
#                              seed=169
# )
# precis(fit_hr_mei_gs_meas_er_6mosshift , depth=2 , c("v_mu" , "sigma_g"))
# precis(fit_hr_mei_gs_meas_er_6mosshift , depth=3 )



#####seasonal models
file_name <- 'stan_code/hr_mei_meas_er_seas.stan' 
fit_seas_1= stan( file = file_name,
                                       data = list_area_seas ,
                                       iter = 4000,
                                       chains=4,
                                       cores=4,
                                       control=list(adapt_delta=0.99) ,
                                       refresh=250,
                                       init=0,
                                       seed=169
)

precis(fit_seas_1, depth=3 , pars='v')
precis(fit_seas_1, depth=3 , pars='Rho_g')
precis(fit_seas_1, depth=3) 

post <- extract.samples(fit_seas_1)
str(post)

for(i in 1:24){
  dens(post$am[,i,1] , col="brown", lty=2 , ylim=c(0,4) , xlim=c(-3,3))
  dens(post$am[,i,2] , col="green", lty=2 , add=TRUE)
  dens(post$am_pred[,i,1] , col="brown", lty=1 , add=TRUE)
  dens(post$am_pred[,i,2] , col="green", lty=1 , add=TRUE)
  points( d_mei_hr_data$mei[d_mei_hr_data$season_index==1 & d_mei_hr_data$year_index_mei==i] , rep(0,4) , col="brown")
  points( d_mei_hr_data$mei[d_mei_hr_data$season_index==2 & d_mei_hr_data$year_index_mei==i] , rep(0,8) , col="green")
}