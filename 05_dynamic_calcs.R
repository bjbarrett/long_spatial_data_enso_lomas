
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

calcs <- list(
  nGroups = length(unique(d_hr_gs$group)), # assuming an `obs` data frame
  nYears = length(unique(d_hr_gs$year)),                            # assuming a `ppl` data frame
  minYear = min(d_hr_gs$year),
  maxYear = max(d_hr_gs$year)
)

min(d_hr_gs$year)
writeLines(prep_latex_variables(calcs), "calcs.tex")

# put this in the tex file's preamble:
# \input{calcs.tex}

# now can write LaTeX things like
# "the number of observations is \nObs,
# the number of people is \nPpl,
# the mean height is \meanHeight."