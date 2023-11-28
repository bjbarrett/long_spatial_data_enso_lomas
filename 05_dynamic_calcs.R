
prep_latex_variables <- function(named_list) {
  out <- character()
  for (i in 1:length(named_list)) {
    out[i] <- paste0("\\newcommand{\\", names(named_list)[i], "}{", named_list[[i]], "}")
  }
  return(out)
}

# calcs <- list(
#   nObs = prettyNum(nrow(obs), big.mark = ","), # assuming an `obs` data frame
#   nPpl = nrow(ppl),                            # assuming a `ppl` data frame
#   meanHeight = round(mean(obs$height), 1)
# )

#below functions makes output ready for tex for point estimates and hpdis

MeanHpdiText <- function(squanch , probability=0.89 ){
  # squanch <- exp(post_hrgs$v_mu[,2])
  mbar <- round( mean(squanch) , 2)
  hpdibar <- round( HPDI(squanch , prob=probability) , 2)
  print(paste0(mbar,"[",hpdibar[[1]],",",hpdibar[[2]],"]" ))
}

MedHpdiText <- function(squanch , probability=0.89 ){
  # squanch <- exp(post_hrgs$v_mu[,2])
  mbar <- round( median(squanch) , 2)
  hpdibar <- round( HPDI(squanch , prob=probability) , 2)
  print(paste0(mbar,"[",hpdibar[[1]],",",hpdibar[[2]],"]" ))
}

calcs <- list(
  nGroups = length(unique(d_hr_gs$group)), # assuming an `obs` data frame
  nYears = length(unique(d_hr_gs$year)),                            # assuming a `ppl` data frame
  minYear = min(d_hr_gs$year),
  maxYear = max(d_hr_gs$year),
  MedHpdiHRA = MedHpdiText(squanch=exp(post_hrgs$v_mu[,1])),
  minMedHpdiHRA = MedHpdiText(squanch=post_hrgs$hr_area_true[,which.min(apply(post_hrgs$hr_area_true , 2 , median))]),
  maxMedHpdiHRA = MedHpdiText(squanch=post_hrgs$hr_area_true[,which.max(apply(post_hrgs$hr_area_true , 2 , median))]),
  betaHRA = MedHpdiText( post_hrgs$v_mu[,2]) ,
  # minMedHpdiBetaHRA = MedHpdiText( squanch=( post_hrgs$v[ , which.min(apply(post_hrgs$v , 2 , median)) , 2] )),
  # maxMedHpdiBetaHRA = MedHpdiText( squanch=(  post_hrgs$v[ , which.max(apply(post_hrgs$v , 2 , median)) , 2] ))
)

post_hrgs$v[ ,which.min(apply(post_hrgs$v , 2 , median)), 2]

min(d_hr_gs$year)
writeLines(prep_latex_variables(calcs), "calcs.tex")

# put this in the tex file's preamble:
# \input{calcs.tex}

# now can write LaTeX things like
# "the number of observations is \nObs,
# the number of people is \nPpl,
# the mean height is \meanHeight."