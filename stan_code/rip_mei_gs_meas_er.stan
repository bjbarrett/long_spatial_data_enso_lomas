// pick up here later
data{
  // mei data
  int N_years;
  vector[12*N_years] mei;
  array[12*N_years] int year_index_mei;
  
  // monkey data
  int N;
  vector [N] prop_river; //shape of each home range estimated by ctmm
  int N_groups;
  array[N] int year_index;
  array[N] int group_index;
  vector[N] group_size_std;

}

parameters{
  //mei parameters
  real<lower=0> sigma; //variation in MEI
  vector[N_years] am; //mein mei per year
  vector[N_years] am_pred; // marginailized posterior predictions of MEI
  // monkey parameters
  matrix[3,N_groups] z_g;
  vector[3] v_mu;
  cholesky_factor_corr[3] L_Rho_g;
  vector<lower=0>[3] sigma_g;
  real<lower=0> theta;
}

transformed parameters{
  matrix[N_groups,3] v;
  v = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
}

model{
    // for mei
  vector[12*N_years] mu;
  am ~ normal( 0 , 2 );
  sigma ~ exponential( 1 );

  for ( j in 1:12*N_years ) {
    mu[j] = am[year_index_mei[j]];
  }

mei ~ normal( mu , sigma );

for (i in 1:N_years) {
  am_pred[i] ~ normal( am[i] , sigma ) ;
}
  vector[N] p;
  sigma_g ~ exponential( 1 );
  L_Rho_g ~ lkj_corr_cholesky( 4 );
  to_vector(v_mu) ~ normal( 0 , 1 );
  theta ~ exponential( 1 );


  to_vector( z_g ) ~ normal( 0 , 1 );

    for ( i in 1:N ) {
      p[i] = v_mu[1] + v[group_index[i],1] 
      + (v_mu[2] + v[group_index[i],2])*am_pred[ year_index[i]]
      + (v_mu[3] + v[group_index[i],3])*group_size_std[i]; 
      p[i] = inv_logit(p[i]);

  }
      prop_river ~ beta( p*theta , (1-p)*theta );

}

generated quantities{
  matrix[3,3] Rho_g;
  Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
}

