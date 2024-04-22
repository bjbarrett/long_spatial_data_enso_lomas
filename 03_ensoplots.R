library(RColorBrewer)
group.pal <- brewer.pal( max(d_hr_gs_3$group_index), "Spectral")
group.pal <- c(group.pal[-6], "#CF9FFF")
dev.off()

# pdf(file="plots/all_da_groups_raw.pdf" , width = 8 , height=11)
# par(mfrow = c(11, 1))
# par(mar = rep(1,4) +0.1, oma = rep(0,4) +0.1)
# #per group plot
# for(i in 1:11){
#   plot(d_mei$mei~d_mei$date , col=elcol_pal[d_mei$phase_index] , pch="x" ,
#        cex=0.5 , ylim=c(-4,4) , main=min(d_hr_gs_3$group[d_hr_gs_3$group_index==i] ) )
#   #lines(mei_spl, col = "grey3")
#   points( d_hr_gs_3$date[d_hr_gs_3$group_index==i] , 
#           standardize(d_hr_gs_3$hr_area_mean[d_hr_gs_3$group_index==i]) , 
#           bg=group_pal[i] , pch=23 , cex=1.4 , col="darkgrey")
#   
#   # datez <- d_hr_gs_3$date[d_hr_gs_3$group_index==i]
#   # gs_stdz_low <- standardize(d_hr_gs_3$hr_area_low[d_hr_gs_3$group_index==i])
#   # gs_stdz_high <- standardize(d_hr_gs_3$hr_area_high[d_hr_gs_3$group_index==i])
#   # 
#   # for(j in 1:length(datez)){
#   # segments( x0=datez[j] , y0=gs_stdz_low[j] , 
#   #          x1=datez[j]  , y1=gs_stdz_high[j] , 
#   #          lty=1 , col=group_pal[i] )
#   # }
#   
# #for (i in 1:33) abline(v=d_mei$date[i+i*11] , col="grey")
# abline(v=d_mei$date[month(d_mei$date)==1] , col="grey")
#   
# }
# dev.off()


###hr post
pdf(file="plots/all_hr_post.pdf" , width =11 , height=13)
  par(mfrow=c(13,10), oma=c(5,0,0,0), mar=c(0,0,0,0) )
  post <- extract.samples(fit_hr_post)
  for (i in 1:130){
    dens(post$hr_area_true[,i] , xlim=c(0,10) ,col="red" , yaxt='n' , xaxt='n')
    dens(rgamma(2000,shape=d_hr_gs_3$shape[[i]], rate=d_hr_gs_3$rate[[i]] ) , add=TRUE )
    lines(density(rgamma(2000,shape=d_hr_gs_3$shape[[i]], scale=d_hr_gs_3$scale[[i]] ) ) , lty=2 )
    points( d_hr_gs_3$area[i] , 0.1 )
    segments(  x0=d_hr_gs_3$low[i], y0=0.1 , x1=d_hr_gs_3$high[i] ,y1= 0.1 , col="blue")
    axis( 1 , at=1:10 , labels=FALSE )
    if(i>120){axis( 1 , at=1:10 , cex.axis=0.7 )}
  }
  mtext(expression(Home ~ range ~ area ~ (km^2)) , side=1, line=4, cex=1.75, outer=TRUE)  
dev.off()

######################################fit_hr
post <- extract.samples(fit)
#post <- extract.samples(fit_hr_mei_meas_er) #different if fit
pdf(file="plots/enso_post.pdf" , width = 10 , height=7)
par(mar = c(0, 0, 0, 0), oma = c(6, 1, 1, 1))
par(mfrow = c(6, 4))
for(i in 1:24){
  dens(post$am_pred[, i ] , ylim=c(0,2.5) , xlim=c(-2.5,2.5) , yaxt='n' , cex.axis=0.8 , xaxt='n')
  dens(post$am[, i ] , lty=2 , add=TRUE)
  points(list_area_2$mei[list_area_2$year_index_mei==i] , rep(0.1,12), 
         col=elcol_pal[list_area_2$phase_index[list_area_2$year_index_mei==i]] 
         , pch=19)
    if (i > 20){axis(1 , at=seq(from=-2,to=2,by=1) )}
    if (i <= 20){axis(1 , at=seq(from=-2,to=2,by=1) , labels=FALSE)}
   title(main=sort(unique(list_area_2$year_mei)[i]) , line=-1)
  
}
mtext(text="Multivariate ENSO index",side=1,line=4,outer=TRUE,cex=1.7)

dev.off()


#########fit_mei_hr_gs#########
post <- extract.samples(fit_hr_mei_gs_meas_er) #different if fit

######plot mean effect across all groups######
pdf(file="plots/m_gs_hr_enso_group_varef.pdf" , width = 10 , height=7)
  par(mfrow = c(3, 4))
  par(cex = 0.6)
  par(mar = c(2.5, 2.5, 0, 0), oma = c(4, 4, 1, 1))
  
  plot(list_area_2$mean_annual_mei,list_area_2$hr_area_mean , ylab="" ,
       xlab="multivariate ENSO index (MEI)" , col="white", xlim=c(min(list_area_2$mei), max(list_area_2$mei)) , ylim=c(0,10.2) )
  title("marginal predictions", line = -1)
  for (obs in 1:list_area_2$N){
    points( post$am_pred[1:100, list_area_2$year_index[obs] ] ,
            rep(list_area_2$hr_area_mean[obs] , 100 ) , col=col.alpha(group.pal[list_area_2$group_index[obs]]), cex=.4)
  }
  
  for(i in 1:nrow(d_hr_gs_3)){
    points(list_area_2$mei[list_area_2$year_index[i] == list_area_2$year_index_mei] , 
           rep(list_area_2$hr_area_mean[i] , 12 ) ,
           col=col.alpha(group.pal[list_area_2$group_index[i] ]) )
  }
  
  seq.mei <- seq(from=min(list_area_2$mei), to=max(list_area_2$mei) , length=30)
  lambda.link <- function(x) exp(post$v_mu[,1] +  post$v_mu[,2] * x +  post$v_mu[,3] *0)
  lambda <- sapply( seq.mei ,lambda.link )
  lambda.mean <- apply( lambda , 2 , mean )
  lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
  for (i in 1:100) {
    lines(seq.mei , lambda[i,] , col=col.alpha(alpha=0.1 ,"black"))
  }
  lines(seq.mei , lambda.mean, col=1 , lw=2)
  
  #####group specific effects
  for(g in 1:max(list_area_2$group_index)){
    plot(list_area_2$mean_annual_mei[list_area_2$group_index==g],
         list_area_2$hr_area_mean[list_area_2$group_index==g] , 
         ylab="" , ylim=c(0,10.2), 
         xlab="" , col="white", 
         xlim=c(min(list_area_2$mei),max(list_area_2$mei)))
    title(min(d_hr_gs_3$group[d_hr_gs_3$group_index==g]), line = -1)
    
    
    for (obs in which(list_area_2$group_index==g) ){
      points( post$am_pred[1:100, list_area_2$year_index[obs] ] ,
              rep(list_area_2$hr_area_mean[obs] , 100 ) , col=col.alpha("grey"), cex=.4 )
    }
    
    for (obs in which(list_area_2$group_index==g) ){
      points( mean(post$am_pred[, list_area_2$year_index[obs] ] ) ,
              list_area_2$hr_area_mean[obs]  , col="grey", cex=1 )
    }

    for (obs in which(list_area_2$group_index==g) ){
      # aich <- HPDI(post$hr_area_true[,obs])
      # segments(x0=mean(post$am_pred[, list_area_2$year_index[obs] ] ) , y0=aich[[1]] , 
      #          x1=mean(post$am_pred[, list_area_2$year_index[obs] ] )  , y1=aich[[2]] , 
      #          lty=1 , col=group.pal[g])
      segments(x0=mean(post$am_pred[, list_area_2$year_index[obs] ] ) , y0=list_area_2$hr_area_low[obs] ,
               x1=mean(post$am_pred[, list_area_2$year_index[obs] ] )  , y1=list_area_2$hr_area_high[obs] ,
               lty=1 , col="grey")
    }
    
    
    seq.mei <- seq(from=min(list_area_2$mei), to=max(list_area_2$mei) , length=30)
    lambda.link <- function(x) exp(post$v_mu[,1] + post$v[,g,1] + (post$v_mu[,2] + post$v[,g,2] )* x + 0*(post$v_mu[,3] + post$v[,g,3] ))
    lambda <- sapply( seq.mei ,lambda.link )
    lambda.mean <- apply( lambda , 2 , mean )
    lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
    for (i in 1:100) {
      lines(seq.mei , lambda[i,] , col=col.alpha(group.pal[g]) , alpha = 0.3)
    }
    lines(seq.mei , lambda.mean, col=group.pal[g] , lw=2)
    # lines(seq.mei , lambda.PI[1,], col=1 , lty=3)
    # lines(seq.mei , lambda.PI[2,], col=1 , lty=3)
  }
  mtext("Multivariate ENSO index (MEI)", side=1, line=1, cex=2, outer=TRUE)  
  mtext(expression(Home ~ range ~ area ~ (km^2)) , side=2, line=0.05, cex=2, outer=TRUE)  

dev.off()

####group size and home range
# ######plot mean effect across all groups######
post <- extract.samples(fit_hr_gs_meas_er)
pdf(file="plots/m_gs_hr_group_varef2.pdf" , width = 10 , height=7)
par(mfrow = c(3, 4))
par(cex = 0.6)
par(mar = c(3, 3, 0, 0), oma = c(4, 4, 1, 1))

plot(list_area_2$group_size_std,list_area_2$hr_area_mean , ylab="" ,
     xlab="" , col=group.pal[list_area_2$group_index],
     xlim=c(min(list_area_2$group_size_std),max(list_area_2$group_size_std)) , ylim=c(0,10.2) ,  xaxt="n"  , pch=19)
title("marginal predictions", line = -1 )
axis( 1 , at= ( seq(from=5 , to=40 , by=5) - mean(list_area_2$group_size))/sd(list_area_2$group_size)
      , labels= seq(from=5 , to=40 , by=5) )
# jittz <- rnorm(mean=0,sd=0.01 , n=list_area_2$N) #to get some seperation on x axis
segments(x0=list_area_2$group_size_std , y0=list_area_2$hr_area_low,
         x1=list_area_2$group_size_std  , y1=list_area_2$hr_area_high ,
         lty=1 , col=group.pal[list_area_2$group_index])

seq.gs <- seq(from=min(list_area_2$group_size_std), to=max(list_area_2$group_size_std) , length=30)
lambda.link <- function(x) exp(post$v_mu[,1] +  post$v_mu[,2] * x )
lambda <- sapply( seq.gs ,lambda.link )
lambda.mean <- apply( lambda , 2 , mean )
lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
for (i in 1:100) {
  lines(seq.mei , lambda[i,] , col=col.alpha(alpha=0.1 ,"black"))
}
lines(seq.gs , lambda.mean, col=1 , lw=2)
lines(seq.gs , lambda.PI[1,], col=1 , lty=3)
lines(seq.gs , lambda.PI[2,], col=1 , lty=3)

#####group specific effects
for(g in 1:max(list_area_2$group_index)){
  plot(list_area_2$group_size_std[list_area_2$group_index==g],list_area_2$hr_area_mean[list_area_2$group_index==g] , ylab="" , pch=19,
       xlab="" , col=group.pal[g], xlim=c(min(list_area_2$group_size_std),max(list_area_2$group_size_std)) , ylim=c(0,10.2) ,  xaxt="n" )
  title(min(d_hr_gs_3$group[d_hr_gs_3$group_index==g]), line = -1)
  axis( 1 , at= ( seq(from=5 , to=40 , by=5) - mean(list_area_2$group_size))/sd(list_area_2$group_size)
        , labels= seq(from=5 , to=40 , by=5) )
  segments(x0=list_area_2$group_size_std[list_area_2$group_index==g] , y0=list_area_2$hr_area_low[list_area_2$group_index==g],
           x1=list_area_2$group_size_std[list_area_2$group_index==g]  , y1=list_area_2$hr_area_high[list_area_2$group_index==g] ,
           lty=1 , col=group.pal[g])
  # seq.gs <- seq(from=min(list_area_2$group_size_std[list_area_2$group_index==g]), to=max(list_area_2$group_size_std[list_area_2$group_index==g]) , length=30)
  seq.gs <- seq(from=min(list_area_2$group_size_std), to=max(list_area_2$group_size_std) , length=30)
  lambda.link <- function(x) exp(post$v_mu[,1] + post$v[,g,1] +( post$v_mu[,2] + post$v[,g,2]) * x )
  lambda <- sapply( seq.gs ,lambda.link )
  lambda.mean <- apply( lambda , 2 , mean )
  lambda.PI <- apply( lambda , 2 , PI , prob=0.89 )
  for (i in 1:100) {
    lines(seq.gs , lambda[i,] , col=col.alpha(alpha=0.1 , group.pal[g]))
  }
  lines(seq.gs , lambda.mean, col=group.pal[g] , lw=2)
}

mtext("Group size", side=1, line=1, cex=2, outer=TRUE)
mtext(expression(Home ~ range ~ area ~ (km^2)) , side=2, line=0.05, cex=2, outer=TRUE)

dev.off()

post <-extract.samples(fit_mei_rip)
str(post)

#######plot mean effect across all groups######
pdf(file="plots/rip_mei_hr_enso_group_varef.pdf" , width = 10 , height=7)
  par(mfrow = c(3, 4))
  par(cex = 0.6)
  par(mar = c(2.5, 2.5, 0, 0), oma = c(4, 4, 1, 1))
  
  plot(drip$mean_mei,drip$prop_river , ylab="" ,
       xlab="" , col="white", xlim=c(-2.5,2.5) , ylim=c(0,.65) )
  title("marginal predictions", line = -1)
  
  for (obs in 1:list_rip$N){
    points( list_rip$mei[drip$year_index[obs] == list_rip$year_index_mei] ,
            rep(list_rip$prop_river[obs] , 12 ) , col=col.alpha(group.pal[list_rip$group_index[obs]]), cex=.4 , pch=1)
  }
  
  for (obs in 1:list_rip$N){
    points( post$am_pred[1:100, list_rip$year_index[obs] ] ,
            rep(list_rip$prop_river[obs] , 100 ) , col=col.alpha(group.pal[list_rip$group_index[obs]]), cex=.4)
    
  }
  
  seq.mei <- seq(from=min(list_rip$mei), to=max(list_rip$mei) , length=30)
  p.link <- function(x) logistic(post$v_mu[,1] + post$v_mu[,2]*x)
  p.dry <- sapply( seq.mei ,p.link )
  p.mean <- apply( p.dry , 2 , mean )
  p.PI <- apply( p.dry , 2 , PI , prob=0.89 )
  for (i in 1:100) {
    lines(seq.mei , p.dry[i,] , col=col.alpha(alpha=0.1 ,"black"))
  }
  lines(seq.mei , p.mean, col="black" , lw=2)
  
  #####group specific effects
  for(g in 1:max(list_rip$group_index)){
    plot(drip$mean_mei,drip$prop_river ,
         ylab="" , xlab="" , col="white",
         xlim=c(-2.5,2.5) , ylim=c(0,.65) )
    title(min(drip$group[drip$group_index==g]), line = -1)

    for (obs in which(list_rip$group_index==g) ){
      points( post$am_pred[1:100, list_rip$year_index[obs] ] ,
              rep(list_rip$prop_river[obs] , 100 ) , col=col.alpha("grey"), cex=.4 )
    }
    
    for (obs in which(list_rip$group_index==g) ){
      points( mean(post$am_pred[, list_rip$year_index[obs] ] ) ,
              list_rip$prop_river[obs]  , col="grey", cex=1 )
    }
      
    seq.mei <- seq(from=min(list_rip$mei), to=max(list_rip$mei) , length=30)
    p.link <- function(x) logistic(  post$v_mu[,1] +  post$v[,g,1] 
                                     + ( post$v_mu[,2] +  post$v[,g,2])*x)
    p.dry <- sapply( seq.mei ,p.link )
    p.mean <- apply( p.dry , 2 , mean )
    p.PI <- apply( p.dry , 2 , PI , prob=0.89 )
    for (i in 1:100) {
      lines(seq.mei , p.dry[i,] , col=col.alpha(alpha=0.1 , group.pal[g]))
    }
    lines(seq.mei , p.mean, col=group.pal[g] , lw=2)

  }
  mtext("Multivariate ENSO index (MEI)", side=1, line=1, cex=2, outer=TRUE)
  mtext("Proportion home range in riparian habitat" , side=2, line=0.05, cex=2, outer=TRUE)

dev.off()

for(g in 1:11){
  dens( (post$v_mu[,2] + post$v[,g,2]) , col=group.pal[g])
}


