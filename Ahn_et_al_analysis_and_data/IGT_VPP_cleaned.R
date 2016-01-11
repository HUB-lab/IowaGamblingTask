# IGT_VPP.R
# Value-Plus-Perseverance Model (Worthy et al. 2014, Frontiers in Psychology)
# HBA with a probit hyperprior
# All codes and data files should be located in the same directory ("saveDir")
# The code assumes trial-by-trial raw data include 'trial', 'deck', 'gain', 'loss', and 'subjID' columnes.
# Programmed by Woo-Young Ahn (www.ahnlab.org)

rm(list=ls(all=TRUE))          	#This clears memory
require(rstan)     # 'http://mc-stan.org/' --> instructions how to install RStan
library(parallel)  # for parallel computing. Install the package first if not installed yet --> 'install.package("parallel")'
library(mail)      # to send emails. Install the package first if not installed yet --> 'install.package("mail")'
library(modeest)   # mode estimation. Install the package first if not installed yet --> 'install.package("modeest")'
set_cppo('fast')   # for best running speed

#################    Variables to customize for each user     #################
#-----------------------------------------------------------------------------#
saveDir = "~/Documents/myDirectory/subMyDirectory/VPP/" # working directory where all codes should exist 
computeWAIC = T       # compute WAIC after run? T (true) or F (false)
parallelComputing = T  # T or F. If T, make sure # of cpus >= nchains
saveFiles = T         # save files? T or F
plotFigs = F           # plot figures? T or F
numPars = 8            # number of parameters    
nchains =  3 # number of chains
nwarmup = 1000 # warm up (discarded)
niter =  3000 # number of iterations (including warmup)
calcIndPars = "mean"    # summarize individual parameters as "mean", "median", or "mode"
payscale = 110 # payscale: divide the raw payoffs by this scale. default: 110 for Frontiers data --> median of immediate payoffs  = 0.5 & 1.0
inits = c(0.1, 0.5, 1.0, 1.0, 0, 0, 0.5, 0.5 )  # inits for parameters (a, alpha, co, lambda, epP, epN, K, w). These can be random numbers
maxTrials = 100 # maximum number of trials
POI = c("mu_A", "mu_alpha", "mu_cons", "mu_lambda", "mu_epP", "mu_epN", "mu_K", "mu_w", 
        "sd_A", "sd_alpha", "sd_cons", "sd_lambda", "sd_epP", "sd_epN", "sd_K", "sd_w", 
        "A", "alpha", "cons", "lambda", "epP", "epN", "K", "w" )   # parameters of interest. NA if estimating all parameters.
modelName = "VPP"
saveName = "IGTdata_healthy_control"  # data file (without .txt extension). It should be a '*.txt* file. 
dataHasHeader = T  # does the data file contain a header (column names)? T or F
colNames = c("trial", "deck", "gain", "loss", "tally", "borrow", "RT", "subjID") # columns of the data file.
sendEmail = T   # send an email to myEmail after the estimation is done? T or F
myEmail = "your_Email@domain.com"  # email to send the notification when the code is finished. 
#-----------------------------------------------------------------------------#

setwd( saveDir )                # go to save directory 
startTime=Sys.time()          	#store the time

## load data
rawdata = read.table( paste( saveName, ".txt", sep=""), header = dataHasHeader)
standColNames= colNames
colnames( rawdata ) = standColNames
subjList =  unique(rawdata[,"subjID"])  # list of subjects x blocks
numSubjs = length(subjList)  # number of subjects

cat("Data file = ", saveName, "\n")
cat("# of MCMC samples = ", niter, "\n")
cat("number of subjects = ", numSubjs, "\n")

#------------------------------------------------------------------------------
# THE MODEL.
modelString = '
data {
  int<lower=1> N;
  int<lower=1> T;
  int<lower=1, upper=T> Tsubj[N];
  real rewlos[N, T];
  int ydata[N, T];
}
transformed data {
}
parameters {
  real mu_A_pr;
  real mu_alpha_pr;
  real mu_cons_pr;
  real mu_lambda_pr;
  real mu_epP_pr;
  real mu_epN_pr;
  real mu_K_pr;
  real mu_w_pr;

  real<lower=0.01, upper=1.5> sd_A;
  real<lower=0.01, upper=1.5> sd_alpha;
  real<lower=0.01, upper=1.5> sd_cons;
  real<lower=0.01, upper=1.5> sd_lambda;
  real<lower=0.01, upper=1.5> sd_epP;
  real<lower=0.01, upper=1.5> sd_epN;
  real<lower=0.01, upper=1.5> sd_K;
  real<lower=0.01, upper=1.5> sd_w;

  real A_pr[N];
  real alpha_pr[N];
  real cons_pr[N];
  real lambda_pr[N];
  real epP_pr[N];
  real epN_pr[N];
  real K_pr[N];
  real w_pr[N];
}
transformed parameters {
  real<lower=0, upper=1> A[N]; 
  real<lower=0, upper=2> alpha[N]; 
  real<lower=0, upper=5> cons[N];   
  real<lower=0, upper=10> lambda[N];  
  real epP[N];
  real epN[N];
  real<lower=0, upper=1> K[N];
  real<lower=0, upper=1> w[N];

  for (i in 1:N) {
    A[i] <- Phi_approx( mu_A_pr + sd_A * A_pr[i] );
    alpha[i] <- 2 * Phi_approx( mu_alpha_pr + sd_alpha * alpha_pr[i] );
    cons[i] <- 5 * Phi_approx( mu_cons_pr + sd_cons * cons_pr[i] );
    lambda[i] <- 10 * Phi_approx( mu_lambda_pr + sd_lambda * lambda_pr[i] );
    epP[i] <- mu_epP_pr + sd_epP * epP_pr[i];
    epN[i] <- mu_epN_pr + sd_epN * epN_pr[i];
    K[i] <- Phi_approx( mu_K_pr + sd_K * K_pr[i] );
    w[i] <- Phi_approx( mu_w_pr + sd_w * w_pr[i] );
  }
}
model {
  mu_A_pr ~ normal(0, 1);
  mu_alpha_pr ~ normal(0, 1);
  mu_cons_pr ~ normal(0, 1);
  mu_lambda_pr ~ normal(0, 1);
  mu_epP_pr ~ normal(0, 5);
  mu_epN_pr ~ normal(0, 5);
  mu_K_pr ~ normal(0, 1);
  mu_w_pr ~ normal(0, 1);

  sd_A ~ uniform(0.01, 1.5);
  sd_alpha ~ uniform(0.01, 1.5);
  sd_cons ~ uniform(0.01, 1.5);
  sd_lambda ~ uniform(0.01, 1.5);
  sd_epP ~ uniform(0.01, 1.5);
  sd_epN ~ uniform(0.01, 1.5);
  sd_K ~ uniform(0.01, 1.5);
  sd_w ~ uniform(0.01, 1.5);

  for (i in 1:N) {
    real ev[4];    
    vector[4] p_next;
    vector[4] str;
    real pers[4];   # perseverance    
    real V[4];   # weighted sum of ev and pers

    real curUtil;     # utility of curFb
    real const_util;  # const of util func
    real selected;    # selected deck
    real theta;       # theta = 3^c - 1

    # Matt trick: all dists. should be normal(0,1)
    A_pr[i] ~ normal(0, 1.0);   
    alpha_pr[i] ~ normal(0, 1.0);   
    cons_pr[i] ~ normal(0, 1.0);   
    lambda_pr[i] ~ normal(0, 1.0); 
    epP_pr[i] ~ normal(0, 1.0); 
    epN_pr[i] ~ normal(0, 1.0); 
    K_pr[i] ~ normal(0, 1.0); 
    w_pr[i] ~ normal(0, 1.0); 
    
    theta <- pow(3, cons[i]) -1;

    for (r in 1:4) {
      ev[r] <- 0;   # initial ev values
      pers[r] <- 0;
    }
    for (t in 1:(Tsubj[i]-1)) {
      for (d in 1:4) {      
        pers[d] <- pers[d] * K[i];   # decay
      }
      if ( rewlos[i,t] >= 0) {  # x(t) >= 0
        curUtil <- pow(rewlos[i,t], alpha[i]);   
        pers[ ydata[i,t] ] <- pers[ ydata[i,t] ] + epP[i];  # perseverance term
      } else {                  # x(t) < 0
        curUtil <- -1 * lambda[i] * pow( -1*rewlos[i,t], alpha[i]);
        pers[ ydata[i,t] ] <- pers[ ydata[i,t] ] + epN[i];  # perseverance term
      }
      
      ev[ ydata[i, t] ] <- ev[ ydata[i, t] ] + A[i] * (curUtil - ev[ ydata[i, t] ] );      
      
      for (d in 1:4) {
        str[d] <- exp( theta * (w[i]*ev[d]+(1-w[i])*pers[d]) );
        #str[d] <- exp( theta * ev[d] );
      }
      for (d in 1:4) {
        p_next[d] <- str[d] / sum(str);
      }
      ydata[i, t+1] ~ categorical( p_next );
    }
  }
}

generated quantities {
  real<lower=0,upper=1> mu_A;   
  real<lower=0,upper=2> mu_alpha;
  real<lower=0,upper=5> mu_cons;  
  real<lower=0,upper=10> mu_lambda;
  real mu_epP;   
  real mu_epN;   
  real<lower=0,upper=1> mu_K;   
  real<lower=0,upper=1> mu_w;   

  mu_A <- Phi_approx(mu_A_pr);
  mu_alpha <- Phi_approx(mu_alpha_pr) * 2;
  mu_cons <- Phi_approx(mu_cons_pr) * 5; 
  mu_lambda <- Phi_approx(mu_lambda_pr) * 10;  
  mu_epP <- mu_epP_pr;
  mu_epN <- mu_epN_pr;
  mu_K <- Phi_approx(mu_K_pr);
  mu_w <- Phi_approx(mu_w_pr);
}
' 

################################################################################
# THE DATA.  ###################################################################
################################################################################

Tsubj = as.vector( rep( 0, numSubjs ) ) # number of trials for each subject 

for ( sIdx in 1:numSubjs )  {
  curSubj = subjList[ sIdx ]
  Tsubj[sIdx] = sum( rawdata$subjID == curSubj )  # Tsubj[N]
}
RLmatrix = array( 0, c(numSubjs, maxTrials ) )
Ydata = array(1, c(numSubjs, maxTrials) )

for ( subjIdx in 1:numSubjs )   {
  #number of trials for each subj.
  useTrials = Tsubj[subjIdx]
  currID = subjList[ subjIdx ]
  rawdata_curSubj = subset( rawdata, subjID == currID )
  
  RLmatrix[subjIdx, 1:useTrials] = rawdata_curSubj[, "gain"] -1 * abs( rawdata_curSubj[ , "loss" ])
  
  for ( tIdx in 1:useTrials ) {
    Y_t = rawdata_curSubj[ tIdx, "deck" ] # chosen Y on trial 't'
    Ydata[ subjIdx , tIdx ] = Y_t
  }
}

dataList = list(
  N = numSubjs,
  T = maxTrials,
  Tsubj = Tsubj ,
  rewlos = RLmatrix / payscale ,
  ydata = Ydata
)

# inits
genInitList <- function() {
  list(
    mu_A_pr = qnorm(inits[1]),
    mu_alpha_pr = qnorm(inits[2]),
    mu_cons_pr = qnorm( inits[3] /5 ),
    mu_lambda_pr = qnorm( inits[4] / 10 ),
    mu_epP_pr = inits[5],
    mu_epN_pr = inits[6],
    mu_K_pr = qnorm(inits[7]),
    mu_w_pr = qnorm(inits[8]),
    sd_A = 1, 
    sd_alpha = 1,
    sd_cons = 1,
    sd_lambda = 1,
    sd_epP = 1, 
    sd_epN = 1, 
    sd_K = 1, 
    sd_w = 1, 
    A_pr = rep( qnorm(inits[1]), numSubjs),
    alpha_pr = rep( qnorm(inits[2]), numSubjs),
    cons_pr = rep( qnorm(inits[3]/5), numSubjs),
    lambda_pr = rep( qnorm(inits[4]/10), numSubjs),
    epP_pr = rep( inits[5], numSubjs),
    epN_pr = rep( inits[6], numSubjs),
    K_pr = rep( qnorm(inits[7]), numSubjs),
    w_pr = rep( qnorm(inits[8]), numSubjs)
  )
}

if (parallelComputing) {
  ### parallel stan call - number of cores = number of chains
  cat( "#### Parallel computing. Fit the model first w/ just 1 sample #### \n")
  fit1 <- stan(model_code = modelString, data=dataList, init = genInitList, iter = 1, chains = 1)  # just 1 sample and 1 chain to initiate
  # Here, data --> dataList!!
  cat( "\n #### Start parallel computing! #### \n\n")
  posterior <- mclapply(1:nchains, mc.cores = nchains, FUN = function(chain) { # or another parallelizing function
    stan(fit = fit1, data = dataList, chains = 1, 
         pars = POI,
         init = genInitList, 
         warmup = nwarmup, iter = niter, 
         verbose = FALSE, refresh = -1, 
         chain_id=chain) 
  } )
  fit <- sflist2stanfit(posterior) # parallel inference!!   
  cat( "\n #### Parallel computing done! #### \n")
} else {
  cat( "#### Serial computing #### \n")
  fit <- stan(model_code = modelString, data = dataList, pars = POI,
              warmup = nwarmup,
              init = genInitList, 
              iter = niter, chains = nchains)  
  cat( "\n #### Serial computing done! #### \n")
}

## Extract parameters
parVals = extract(fit, permuted=T)

aHypSample = parVals$mu_A
alphaHypSample = parVals$mu_alpha
cHypSample = parVals$mu_cons
lambdaHypSample = parVals$mu_lambda
epPHypSample = parVals$mu_epP
epNHypSample = parVals$mu_epN
kHypSample = parVals$mu_K
wHypSample = parVals$mu_w

sd_A = parVals$sd_A
sd_alpha = parVals$sd_alpha
sd_cons = parVals$sd_cons
sd_lambda = parVals$sd_lambda
sd_epP = parVals$sd_epP
sd_epN = parVals$sd_epN
sd_K = parVals$sd_K
sd_w = parVals$sd_w

A = parVals$A
alpha = parVals$alpha
cons = parVals$cons
lambda = parVals$lambda
epP = parVals$epP
epN = parVals$epN
K = parVals$K
w = parVals$w

# Individual parameters (e.g., individual posterior means)
allIndPars = array(NA, c(numSubjs, numPars))
allIndPars = as.data.frame(allIndPars)
for (i in 1:numSubjs) {
  if (calcIndPars=="mean") {
    allIndPars[i, ] = c( mean(A[, i]), mean(alpha[, i]), mean(cons[, i]), mean(lambda[, i]), mean(epP[, i]), mean(epN[, i]), mean(K[, i]), mean(w[, i]) )    
  } else if (calcIndPars=="median") {
    allIndPars[i, ] = c( median(A[, i]), median(alpha[, i]), median(cons[, i]), median(lambda[, i]), median(epP[, i]), median(epN[, i]), median(K[, i]), median(w[, i]) )    
  } else if (calcIndPars=="mode") {
    allIndPars[i, ] = c( mlv(A[, i], method="shorth")[1], 
                         mlv(alpha[, i], method="shorth")[1],
                         mlv(cons[, i], method="shorth")[1],
                         mlv(lambda[, i], method="shorth")[1],
                         mlv(epP[, i], method="shorth")[1], 
                         mlv(epN[, i], method="shorth")[1],
                         mlv(K[, i], method="shorth")[1], 
                         mlv(w[, i], method="shorth")[1] )
  }   
}  
allIndPars = cbind(allIndPars, subjList)
colnames(allIndPars) = c("A", "alpha", "c", "lambda",
                         "epP", "epN", "K", "w",
                         "subjID")  



if (plotFigs) {
  x11()           # x11 works for all operating systems..
  traceplot(fit)  # traceplot

  # Plot hyper-parameters
  x11(width=8, height=3)   # x11 works for all operating systems..
  layout( matrix(1:numPars, 1, numPars, byrow=F))
  hist(aHypSample, main=round(mean(aHypSample),3), freq=F, xlab="A" )
  hist(alphaHypSample, main=round(mean(alphaHypSample),3), freq=F, xlab=expression(alpha) )
  hist(cHypSample, main=round(mean(cHypSample),3), freq=F, xlab=expression(c) )
  hist(lambdaHypSample, main=round(mean(lambdaHypSample),3), freq=F, xlab=expression(lambda))
  hist(epPHypSample, main=round(mean(epPHypSample),3), freq=F, xlab="epP" )
  hist(epNHypSample, main=round(mean(epNHypSample),3), freq=F, xlab="epN" )
  hist(kHypSample, main=round(mean(kHypSample),3), freq=F, xlab="K" )
  hist(wHypSample, main=round(mean(wHypSample),3), freq=F, xlab=expression(omega) )
  
}

# Print out Stan results
print(fit)
endTime = Sys.time()
timeTook = endTime - startTime  # time took to run the code

if (sendEmail) {
  # send mail - check the email address below!!
  sendmail(myEmail, paste("model=", modelName, ", fileName = ", saveName),
           paste("Check ", getwd(), ". It took ", as.character.Date(timeTook), sep="") )
}

# Compute WAIC
if (computeWAIC) {
  source("WAIC_VPP_cleaned.R")
}

# end of the code
