data{
  int N;
  vector[N] overlap_uds;
  array[N] int g1_index;
  array[N] int g2_index;
  array[N] int d_index;
  int N_groups;
  int N_dyads;
}
parameters{
  real o_bar;
  real<lower=0> theta;
  vector[N_groups] g;
  vector[N_dyads] d;
  real<lower=0> sigma_g;
  real<lower=0> sigma_d;
}

model{
vector[N] p;
theta ~ exponential( 1 );
o_bar ~ normal( 0 , 1 );
g ~ normal( 0 , sigma_g );
d ~ normal( 0 , sigma_d );
sigma_g ~ exponential( 1 );
sigma_d ~ exponential( 1 );

  for ( i in 1:N ) {
       p[i] = o_bar + g[g1_index[i]] + g[g2_index[i]] + d[d_index[i]] ;
       p[i] = inv_logit(p[i]);
  }
  overlap_uds ~ beta( p*theta , (1-p)*theta );
}
