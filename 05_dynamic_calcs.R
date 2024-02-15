
prep_latex_variables <- function(named_list) {
  out <- character()
  for (i in 1:length(named_list)) {
    out[i] <- paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
  }
  return(out)
}

PropPostPosi <- function(x , digits=3){
  round( length(which(x > 0))/length(x) , digits )
}

#below functions makes output ready for tex for point estimates and hpdis

MeanHpdiText <- function(squanch , probability=0.89 ){
  # squanch <- exp(post_hrgs$v_mu[,2])
  mbar <- round( mean(squanch) , 2)
  hpdibar <- round( HPDI(squanch , prob=probability) , 2)
  print(paste0(mbar," [",hpdibar[[1]],",",hpdibar[[2]],"]" ))
}

MedHpdiText <- function(squanch , probability=0.89 ){
  # squanch <- exp(post_hrgs$v_mu[,2])
  mbar <- round( median(squanch) , 2)
  hpdibar <- round( HPDI(squanch , prob=probability) , 2)
  print(paste0(mbar," [",hpdibar[[1]],",",hpdibar[[2]],"]" ))
}

sss <- post_hrgs$v[,,2] + post_hrgs$v_mu[,2]
ttt <- post_hrgsmei$v[,,2] + post_hrgsmei$v_mu[,2]
uuu <-  post_meirip$v[,,2] + post_meirip$v_mu[,2]
apply(sss,2,mean)
which.max(apply(sss,2,mean))

for( i in 1:11) dens(sss[,i] , title=i)

calcs <- list(
  nGroups = length(unique(d_hr_gs$group)), # assuming an `obs` data frame
  nYears = length(unique(d_hr_gs$year)),                            # assuming a `ppl` data frame
  minYear = min(d_hr_gs$year),
  maxYear = max(d_hr_gs$year),
  MedHpdiHRA = MedHpdiText(squanch=exp(post_hrgs$v_mu[,1])),
  minMedHpdiHRA = MedHpdiText(squanch=post_hrgs$hr_area_true[,which.min(apply(post_hrgs$hr_area_true , 2 , median))]),
  maxMedHpdiHRA = MedHpdiText(squanch=post_hrgs$hr_area_true[,which.max(apply(post_hrgs$hr_area_true , 2 , median))]),
  betaHRA = MedHpdiText( post_hrgs$v_mu[,2]) ,
  propPosiHRA= PropPostPosi(post_hrgs$v_mu[,2]) ,
  minMedHpdiBetaHRA = MedHpdiText( squanch=(sss[,which.min(apply(sss,2,mean))])),
  maxMedHpdiBetaHRA = MedHpdiText( squanch=sss[,which.max(apply(sss,2,mean))]),
  betaMEI = MedHpdiText( post_hrgsmei$v_mu[,2]) ,
  propPosiMEI= PropPostPosi(post_hrgsmei$v_mu[,2]) ,
  minMedHpdiBetaMEI = MedHpdiText( squanch=(ttt[,which.min(apply(ttt,2,mean))])),
  maxMedHpdiBetaMEI = MedHpdiText( squanch=ttt[,which.max(apply(ttt,2,mean))]),
  betaMEIrip = MedHpdiText( post_meirip$v_mu[,2]) ,
  minMedHpdiBetaMEIrip = MedHpdiText( squanch=(uuu[,which.min(apply(uuu,2,mean))])),
  maxMedHpdiBetaMEIrip = MedHpdiText( squanch=(uuu[,which.max(apply(uuu,2,mean))])) ,
  propPosiMEIrip = PropPostPosi(post_meirip$v_mu[,2])
)



min(d_hr_gs$year)
writeLines(prep_latex_variables(calcs), "calcs.tex")
