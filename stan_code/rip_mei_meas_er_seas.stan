// pick up here later
data{
  // mei data
  vector[96] mei_dry;
  array[96] int year_index_mei_dry;
  vector[192] mei_wet;
  array[192] int year_index_mei_wet;
  int N_years;
  // monkey data
  int N;
  array [N] int hr_area; //shape of each home range estimated by ctmm
  array [N] int intersect_area; //rate of each home range estimated by ctmm
  int N_groups;
  array[N] int year_index;
  array[N] int group_index;
  array[N] int wet;
  array[N] int season_index;
}

parameters{
  //mei parameters
  real<lower=0> sigma_d; //variation in MEI dry season
  real<lower=0> sigma_w; //variation in MEI wet season
  matrix[N_years,2] am; //mein mei per year
  matrix[N_years,2] am_pred; // marginailized posterior predictions of MEI
  // monkey parameters
  matrix[4,N_groups] z_g;
  real a;
  real bm;
  real bw;
  real bwXm;
  cholesky_factor_corr[4] L_Rho_g;
  vector<lower=0>[4] sigma_g;
}

transformed parameters{
  matrix[N_groups,4] v;
  v = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
}

model{
  // for mei
  vector[N_years*8] mu_w;
  vector[N_years*4] mu_d;
  to_vector(am) ~ normal( 0 , 2 );
  sigma_d ~ exponential( 1 );
  sigma_w ~ exponential( 1 );

  for ( i in 1:N_years*4 ) {
    mu_d[i] = am[year_index_mei_dry[i],1];
  }
  for ( i in 1:N_years*8 ) {
    mu_w[i] = am[year_index_mei_wet[i],2];
  }

  mei_dry ~ normal( mu_d , sigma_d );
  mei_wet ~ normal( mu_w , sigma_w );

for (i in 1:N_years) {
     am_pred[i,1] ~ normal( am[i,1] , sigma_d ) ;
     am_pred[i,2] ~ normal( am[i,2] , sigma_w ) ;
}

  vector[N] p;
  sigma_g ~ exponential( 1 );
  L_Rho_g ~ lkj_corr_cholesky( 3 );
  a ~ normal( 0 , 1 );
  bw ~ normal( 0 , 1 );
  bm ~ normal( 0 , 1 );
  bwXm ~ normal( 0 , 1 );

  to_vector( z_g ) ~ normal( 0 , 1 );

    for ( i in 1:N ) {
      p[i] = a + v[group_index[i],1] + (bw + v[group_index[i],2])*wet[i] 
      + (bm + v[group_index[i],3])*am[ year_index[i] , season_index[i] ] 
      + (bwXm + v[group_index[i],4]) * wet[i] * am[year_index[i] , season_index[i] ];
  }
      intersect_area ~ binomial_logit( hr_area , p );
}

generated quantities{
  matrix[4,4] Rho_g;
  Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
}

