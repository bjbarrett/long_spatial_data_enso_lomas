data{
  // mei data
    int N_years ;
    int N_mei ;
    vector[N_mei] mei;
    array[N_mei] int year_index_mei;

  // monkey data
}
parameters{
  //mei parameters
     real<lower=0> sigma;
     vector[N_years] am;
     vector[N_years] am_pred;
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
}