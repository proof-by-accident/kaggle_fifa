//
data {
     int N_full;
     int N_train;
     int N_test;
     
     int M;
     
     int y_full[N_full];
     int y_train[N_train];	
     int y_test[N_test];

     matrix[N_full,M] x_full;
     matrix[N_train,M] x_train;
     matrix[N_test,M] x_test;
     
}

//
parameters {
	   vector[M] beta;
	   real beta0;

	   vector[M] beta_full;
	   real beta0_full;
}

//
model {
      beta0 ~ double_exponential( 0, 10  );
      beta0_full ~ double_exponential( 0, 10  );

      beta ~ double_exponential( 0, 10  );
      beta_full ~ double_exponential( 0, 10  );

      y_train ~ bernoulli_logit( beta0 + x_train * beta );
      y_full ~ bernoulli_logit( beta0_full + x_full * beta_full );
}

//
generated quantities {
	  vector[N_test] p_test;
	  p_test = inv_logit( beta0 + x_test * beta );

}

