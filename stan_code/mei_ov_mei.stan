data{
  // mei data
    int N_years ;
    int N_mei ;
    vector[N_mei] mei;
    array[N_mei] int year_index_mei;

  //monkeydata
  int N;
  vector[N] overlap_uds;
  array[N] int g1_index;
  array[N] int g2_index;
  array[N] int d_index;
  array[N] int year_index;
  int N_groups;
  int N_dyads;
}

parameters{
  //mei parameters
     real<lower=0> sigma;
     vector[N_years] am;
     vector[N_years] am_pred;
  // monkey params
  real o_bar;
  real bm;
  real<lower=0> theta;
  
  matrix[2,N_groups] z_g;
  cholesky_factor_corr[2] L_Rho_g;
  vector<lower=0>[2] sigma_g;

  matrix[2,N_dyads] z_d;
  cholesky_factor_corr[2] L_Rho_d;
  vector<lower=0>[2] sigma_d;
/*  matrix[N_groups,2] g;
  matrix[N_dyads,2] d;*/


}

transformed parameters{
  matrix[N_groups,2] g;
  g = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
  matrix[N_dyads,2] d;
  d = (diag_pre_multiply(sigma_d, L_Rho_d) * z_d)';
}

model{
 // for mei
    vector[N_mei] mu;
    am ~ normal( 0 , 2 );
    sigma ~ exponential( 1 );
    for ( i in 1:N_mei ) {
        mu[i] = am[year_index_mei[i]];
    }
    mei ~ normal( mu , sigma );
    for (i in 1:N_years) {
      am_pred[i] ~ normal( am[i] , sigma ) ;
    }

  vector[N] p;
  theta ~ exponential( 1 );
  o_bar ~ normal( 0 , 1 );
  bm  ~ normal( 0 , 1 );
  L_Rho_g ~ lkj_corr_cholesky( 3 );
  L_Rho_d ~ lkj_corr_cholesky( 3 );
  to_vector( z_g ) ~ normal( 0 , 1 );
  to_vector( z_d ) ~ normal( 0 , 1 );
  to_vector(sigma_g) ~ exponential( 1 );
  to_vector(sigma_d) ~ exponential( 1 );

  for ( i in 1:N ) {
       p[i] = o_bar + g[g1_index[i],1] + g[g2_index[i],1] + d[d_index[i],1] + 
              (bm + g[g1_index[i],2] + g[g2_index[i],2] + d[d_index[i],2] ) * am_pred[year_index[i]];
       p[i] = inv_logit(p[i]);
  }
  overlap_uds ~ beta( p*theta , (1-p)*theta );
}

generated quantities{
  matrix[2,2] Rho_g;
  Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
  matrix[2,2] Rho_d;
  Rho_d = multiply_lower_tri_self_transpose(L_Rho_d);
}
