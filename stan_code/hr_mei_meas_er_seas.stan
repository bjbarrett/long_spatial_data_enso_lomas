// pick up here later
data{
  // mei data
  vector[96] mei_dry;
  array[96] int year_index_mei_dry;
  vector[192] mei_wet;
  array[192] int year_index_mei_wet;
  int N_years;
  // array[288] int season_index_mei;
  // monkey data
/*  int N;
  vector[N] kde_shape; //shape of each home range estimated by ctmm
  vector[N] kde_rate; //rate of each home range estimated by ctmm
  int N_groups;
  array[N] int year_index;
  array[N] int group_index;
  array[N] int season_index;*/
}

parameters{
  //mei parameters
  real<lower=0> sigma_d; //variation in MEI dry season
  real<lower=0> sigma_w; //variation in MEI wet season
  matrix[N_years,2] am; //mein mei per year
  matrix[N_years,2] am_pred; // marginailized posterior predictions of MEI
  // //monkey parameters
  // matrix[3,N_groups] z_g;
  // vector[3] v_mu;
  // //vector[2] b_seas;
  // 
  // cholesky_factor_corr[3] L_Rho_g;
  // vector<lower=0>[3] sigma_g;
  // real<lower=0> k;
  // vector<lower=0>[N] hr_area_true; //estimated shape and range
}

// transformed parameters{
//   matrix[N_groups,3] v;
//   v = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
// }

model{
  // for mei
  vector[N_years*8] mu_w;
  vector[N_years*4] mu_d;
  to_vector(am) ~ normal( 0 , 2 );
  sigma_d ~ exponential( 1 );
  sigma_w ~ exponential( 1 );
  // vector[N_years*12] mu_wet;
  // to_vector(am_wet) ~ normal( 0 , 2 );
  // sigma_wet ~ exponential( 1 );
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

  // vector[N] lambda;
  // vector[N] hr_area_obs; //storage value for posterior of home ranges
  // k ~ exponential(1); // prior on scale
  // sigma_g ~ exponential( 1 );
  // L_Rho_g ~ lkj_corr_cholesky( 3 );
  // v_mu[1] ~ normal( 1 , 0.8 );
  // v_mu[2] ~ normal( 1 , 0.8 );
  // v_mu[3] ~ normal( 0 , 1 );

  //to_vector(b_seas) ~ normal( 0 , 1 );
/*  to_vector( z_g ) ~ normal( 0 , 1 );

  for ( i in 1:N ) {
      hr_area_true[i] ~ gamma( kde_shape[i] , kde_rate[i]); // estimate posteriors of home range for each observation
  }

  hr_area_obs=hr_area_true; // store posterior as something else to make outcomes

    for ( i in 1:N ) {
      lambda[i] = v_mu[season_index[i] ]  + v[group_index[i] , season_index[i] ]
       (v_mu[3] + v[group_index[i], 3]) * am_pred[year_index[i]] ;
      lambda[i] = exp(lambda[i]);
  }
  hr_area_obs ~ gamma( lambda/k , 1/k );*/
}

// generated quantities{
//   matrix[3,3] Rho_g;
//   Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
// }


// // below gets wet and dry means per month
// data{
//   // monkey data
//   int N;
//   vector[N] kde_shape; //shape of each home range estimated by ctmm
//   vector[N] kde_rate; //rate of each home range estimated by ctmm
//   int N_groups;
//   array[N] int year_index;
//   array[N] int group_index;
//   array[N] int season_index;
// }
// 
// parameters{
//   //monkey parameters
//   matrix[2,N_groups] z_g;
//   vector[2] v_mu;
//   //vector[2] b_seas;
// 
//   cholesky_factor_corr[2] L_Rho_g;
//   vector<lower=0>[2] sigma_g;
//   real<lower=0> k;
//   vector<lower=0>[N] hr_area_true; //estimated shape and range
// }
// 
// transformed parameters{
//   matrix[N_groups,2] v;
//   v = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
// }
// 
// model{
// 
//   vector[N] lambda;
//   vector[N] hr_area_obs; //storage value for posterior of home ranges
//   k ~ exponential(1); // prior on scale
//   sigma_g ~ exponential( 1 );
//   L_Rho_g ~ lkj_corr_cholesky( 3 );
//   v_mu[1] ~ normal( 1 , 0.8 );
//   v_mu[2] ~ normal( 1 , 0.8 );
//   //to_vector(b_seas) ~ normal( 0 , 1 );
//   to_vector( z_g ) ~ normal( 0 , 1 );
// 
//   for ( i in 1:N ) {
//       hr_area_true[i] ~ gamma( kde_shape[i] , kde_rate[i]); // estimate posteriors of home range for each observation
//   }
// 
//   hr_area_obs=hr_area_true; // store posterior as something else to make outcomes
// 
//     for ( i in 1:N ) {
//       lambda[i] = v_mu[season_index[i] ]  + v[group_index[i] , season_index[i] ] ;
//       lambda[i] = exp(lambda[i]);
//   }
//   hr_area_obs ~ gamma( lambda/k , 1/k );
// }
// 
// generated quantities{
//   matrix[2,2] Rho_g;
//   Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
// }

// 
// // below is wet dry season means 
// 
// // data{
// //   // monkey data
// //   int N;
// //   vector[N] kde_shape; //shape of each home range estimated by ctmm
// //   vector[N] kde_rate; //rate of each home range estimated by ctmm
// //   int N_groups;
// //   array[N] int year_index;
// //   array[N] int group_index;
// //   array[N] int season_index;
// // }
// // 
// // parameters{
// //   //monkey parameters
// //   matrix[2,N_groups] z_g;
// //   vector[2] v_mu;
// //   vector[2] b_seas;
// // 
// //   cholesky_factor_corr[2] L_Rho_g;
// //   vector<lower=0>[2] sigma_g;
// //   real<lower=0> k;
// //   vector<lower=0>[N] hr_area_true; //estimated shape and range
// // }
// // 
// // transformed parameters{
// //   matrix[N_groups,2] v;
// //   v = (diag_pre_multiply(sigma_g, L_Rho_g) * z_g)';
// // }
// // 
// // model{
// //   
// //   vector[N] lambda; 
// //   vector[N] hr_area_obs; //storage value for posterior of home ranges
// //   k ~ exponential(1); // prior on scale
// //   sigma_g ~ exponential( 1 );
// //   L_Rho_g ~ lkj_corr_cholesky( 3 );
// //   v_mu[1] ~ normal( 1 , 0.8 );
// //   v_mu[2] ~ normal( 0 , 1 );
// //   to_vector(b_seas) ~ normal( v_mu[1] , 1 );
// //   to_vector( z_g ) ~ normal( 0 , 1 );
// //   
// //   for ( i in 1:N ) {
// //       hr_area_true[i] ~ gamma( kde_shape[i] , kde_rate[i]); // estimate posteriors of home range for each observation
// //   }
// //   
// //   hr_area_obs=hr_area_true; // store posterior as something else to make outcomes
// //   
// //     for ( i in 1:N ) {
// //       lambda[i] = v[group_index[i], 1] + b_seas[season_index[i]];
// //       lambda[i] = exp(lambda[i]);
// //   }
// //   hr_area_obs ~ gamma( lambda/k , 1/k );
// // }
// // 
// // generated quantities{
// //   matrix[2,2] Rho_g;
// //   Rho_g = multiply_lower_tri_self_transpose(L_Rho_g);
// // }
