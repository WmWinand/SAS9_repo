ods graphics on;

/* EXAMPLE 1 */

/*-----------------------------------------------------------------
            S A S   S A M P L E   L I B R A R Y                   
                                                                  
      NAME: nlmgs1                                                
     TITLE: Getting Started Example 1 for PROC NLMIXED            
   PRODUCT: STAT                                                  
    SYSTEM: ALL                                                   
      KEYS: Logistic growth curve with random effects             
            Growth data                                           
            Continuous response                                   
     PROCS: NLMIXED                                               
      DATA: Orange tree data, Draper and Smith (1981)             
                                                                  
   SUPPORT: Oliver Schabenberger                                  
       REF:                                                       
      MISC:                                                       
-----------------------------------------------------------------*/

data tree;
  input tree day y;
  datalines;
1  118   30
1  484   58
1  664   87
1 1004  115
1 1231  120
1 1372  142
1 1582  145
2  118   33
2  484   69
2  664  111
2 1004  156
2 1231  172
2 1372  203
2 1582  203
3  118   30
3  484   51
3  664   75
3 1004  108
3 1231  115
3 1372  139
3 1582  140
4  118   32
4  484   62
4  664  112
4 1004  167
4 1231  179
4 1372  209
4 1582  214
5  118   30
5  484   49
5  664   81
5 1004  125
5 1231  142
5 1372  174
5 1582  177
;

proc print data=tree;
run;

proc nlmixed data=tree;
   parms b1=190 b2=700 b3=350 s2u=1000 s2e=60;
   num = b1+u1;
   ex  = exp(-(day-b2)/b3);
   den = 1 + ex;
   model y ~ normal(num/den,s2e);
   random u1 ~ normal(0,s2u) subject=tree;
run;


/* EXAMPLE 2 */

/*-----------------------------------------------------------------
            S A S   S A M P L E   L I B R A R Y                   
                                                                  
      NAME: nlmgs2                                                
     TITLE: Getting Started Example 2 for PROC NLMIXED            
   PRODUCT: STAT                                                  
    SYSTEM: ALL                                                   
      KEYS: Logistic-normal model                                 
            Binomial data                                         
            Multi-center clinical trial                           
     PROCS: NLMIXED                                               
      DATA: Beitler and Landis (1985)                             
                                                                  
   SUPPORT: Oliver Schabenberger                                  
       REF:                                                       
      MISC:                                                       
-----------------------------------------------------------------*/

data infection;
  input clinic t x n;
  datalines;
1 1 11 36
1 0 10 37
2 1 16 20
2 0 22 32
3 1 14 19
3 0  7 19
4 1  2 16
4 0  1 17
5 1  6 17
5 0  0 12
6 1  1 11
6 0  0 10
7 1  1  5
7 0  1  9
8 1  4  6
8 0  6  7
;

proc print data=infection;
run;

proc nlmixed data=infection;
   parms beta0=-1 beta1=1 s2u=2;
   eta    = beta0 + beta1*t + u;
   expeta = exp(eta);
   p      = expeta/(1+expeta);
   model x ~ binomial(n,p);
   random u ~ normal(0,s2u) subject=clinic;
   predict eta out=eta;
   estimate '1/beta1' 1/beta1;
run;

ods graphics off;
