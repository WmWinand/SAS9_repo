ODS GRAPHICS ON;

/* EXAMPLE 1: SIMPLE LINEAR REGRESSION MODEL */
proc print data=sashelp.class;
run;

proc mcmc data=SasHelp.class seed=1 nbi=5000 nmc=10000 outpost=regOut;
	parms beta0 beta1 sigma2;
	prior beta: ~ normal(0, var=100);
	prior sigma2 ~ igamma(shape=2, scale=2);
	mu=beta0+beta1*height;
	model weight ~ normal(mu, var=sigma2);
run;

/* EXAMPLE 2: GENERALIZED LINEAR REGRESSION MODELS WITH LINK FUNCTIONS */
data Beetles;
	input n y x @@;
	datalines;
6 0 25.7 8 2 35.9 5 2 32.9 7 7 50.4 6 0 28.3
7 2 32.3 5 1 33.2 8 3 40.9 6 0 36.5 6 1 36.5
6 6 49.6 6 3 39.8 6 4 43.6 6 1 34.1 7 1 37.4
8 2 35.2 6 6 51.3 5 3 42.5 7 0 31.3 3 2 40.6
;

proc print data=Beetles;
run;

proc mcmc data=Beetles seed=2 nmc=20000 nthin=2 outpost=Beetlesout;
	parms alpha beta;
	prior alpha beta ~ normal(0, var=10000);
	p=logistic(alpha+beta*x);
	model y ~ binomial(n, p);
run;

/* EXAMPLE 3: NONLINEAR REGRESSION MODEL */
data calls;
	input weeks calls @@;
	datalines;
1 0 1 2 2 2 2 1 3 1 3 3
4 5 4 8 5 5 5 9 6 17 6 9
7 24 7 16 8 23 8 27
;

proc print data=calls;
run;

proc mcmc data=calls nmc=20000 seed=53197 outpost=callsout propcov=quanew;
	parms alpha -4 beta 1 gamma 2;
	prior gamma ~ gamma(3.5, scale=12);
	prior alpha ~ normal(-5, sd=0.25);
	prior beta ~ normal(0.75, sd=0.5);
	lambda=gamma*logistic(alpha+beta*weeks);
	model calls ~ poisson(lambda);
run;

ODS GRAPHICS OFF;