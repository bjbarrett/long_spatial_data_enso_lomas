library(xtable)
## fit_hr_gs
xx <- precis(fit_hr_gs_meas_er , depth=3 , pars=c("v_mu", "sigma_g" , "k" , "Rho_g"))
colnames(xx) <- c( "mean","standard deviation","5.5\\%","94.5\\%","n\\_eff" ,"Rhat")
xx <- xx[1:7,1:6]
xx <- xx[-6,1:6]
row.names(xx) <- c("$\\alpha$" , "$\\beta_{GS}$" , "$\\sigma_{\\alpha}$" ,
                   "$\\sigma_{beta_{GS}}$"  , "$k$" , "$\\rho_{\\alpha_g,\\beta_{GS}}$")

yams <- xtable(xx ,
               digits=2 , # number decimal places
               label="tab:precis_hr_gs_me" , #label to call item in latex
               caption=c("Summary statistics of parameters from model  \\texttt{fit\\_hr\\_gs\\_meas\\_er} including posterior mean, standard deviation, 89\\% HPDI , number of effective samples, and Rhat (indicating convergence).") ,
               auto=TRUE) #caption in figure
print(yams , file="precis_hr_gs_me.tex" , sanitize.text.function=function(x){x} ) #sanitize text makes math mode latex friendly


####### fit_hr_gs_mei
xx <- precis(fit_hr_mei_gs_meas_er , depth=3 , pars=c("v_mu", "sigma_g" , "k" , "Rho_g" ))
colnames(xx) <- c( "mean","standard deviation","5.5\\%","94.5\\%","n\\_eff" ,"Rhat")
xx <- xx[1:13,1:6]
xx <- xx[-8,1:6]
xx <- xx[-10:-11,1:6]
 
row.names(xx) <- c("$\\alpha$" , "$\\beta_{ENSO}$" , "$\\beta_{GS}$" , "$\\sigma_{\\alpha}$" , "$\\sigma_{\\beta_{ENSO}}$",
                       "$\\sigma_{beta_{GS}}$"  , "$k$" , "$\\rho_{\\alpha_g,\\beta_{ENSO}}$" , 
                       "$\\rho_{\\alpha_g,\\beta_{GS}}$" , "$\\rho_{\\beta_{ENSO},\\beta_{GS}}$")
yams <- xtable(xx ,
               digits=2 , # number decimal places
               label="tab:precis_hr_mei_gs_me" , #label to call item in latex
               caption=c("Summary statistics of parameters from model  \\texttt{fit\\_hr\\_mei\\_gs\\_meas\\_er} including posterior mean, standard deviation, 89\\% HPDI , number of effective samples, and Rhat (indicating convergence).") ,
               auto=TRUE) #caption in figure
print(yams , file="precis_hr_mei_gs_me.tex" , sanitize.text.function=function(x){x} ) #sanitize text makes math mode latex friendly



## fit_hr_mei
xx <- precis(fit_hr_mei_meas_er , depth=3 , pars=c("v_mu", "sigma_g" , "k" , "Rho_g"))
colnames(xx) <- c( "mean","standard deviation","5.5\\%","94.5\\%","n\\_eff" ,"Rhat")
xx <- xx[1:7,1:6]
xx <- xx[-6,1:6]
row.names(xx) <- c("$\\alpha$" , "$\\beta_{ENSO}$" , "$\\sigma_{\\alpha}$" ,
                   "$\\sigma_{beta_{ENSO}}$"  , "$k$" , "$\\rho_{\\alpha_g,\\beta_{ENSO}}$")

yams <- xtable(xx ,
               digits=2 , # number decimal places
               label="tab:precis_hr_mei_me" , #label to call item in latex
               caption=c("Summary statistics of parameters from model \\texttt{fit\\_hr\\_mei\\_meas\\_er} including posterior mean, standard deviation, 89\\% HPDI , number of effective samples, and Rhat (indicating convergence).") ,
               auto=TRUE) #caption in figure
print(yams , file="precis_hr_mei_me.tex" , sanitize.text.function=function(x){x} ) #sanitize text makes math mode latex friendly



## fit_mei_rip
precis(fit_mei_rip , depth=3 , pars=c("v_mu", "sigma_g" , "theta" ,"Rho_g" ))
xx <- precis(fit_mei_rip , depth=3 , pars=c("v_mu", "sigma_g" , "theta" ,"Rho_g" ))
colnames(xx) <- c("mean","standard deviation","5.5\\%","94.5\\%","n\\_eff" ,"Rhat")

xx <- xx[1:7,1:6]
xx <- xx[-6,1:6]
row.names(xx) <- c("$\\alpha$" , "$\\beta_{ENSO}$" , "$\\sigma_{\\alpha}$" ,
"$\\sigma_{beta_{ENSO}}$"  , "$\\theta$" , "$\\rho_{\\alpha_g,\\beta_{ENSO}}$")

yams <- xtable(xx ,
               digits=2 , # number decimal places
               label="tab:precis_mei_rip_me" , #label to call item in latex
               caption=c("Summary statistics of parameters from model \\texttt{fit\\_mei\\_rip} including posterior mean, standard deviation, 89\\% HPDI , number of effective samples, and Rhat (indicating convergence).") ,
               auto=TRUE) #caption in figure
print(yams , file="precis_mei_rip_me.tex" , sanitize.text.function=function(x){x} ) #sanitize text makes math mode latex friendly

