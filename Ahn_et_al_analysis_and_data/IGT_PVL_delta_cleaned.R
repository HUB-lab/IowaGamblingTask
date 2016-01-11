# IGT_PVL_decayRI.R
# PVL model with delta learning rule (Ahn et al., 2008, Cog Sci; 2011, JNPE)
# HBA with a probit hyperprior
# All codes and data files should be located in the same directory ("saveDir")
# The code assumes trial-by-trial raw data include 'trial', 'deck', 'gain', 'loss', and 'subjID' columnes.
# Programmed by Woo-Young Ahn (www.ahnlab.org)

rm(list=ls(all=TRUE))      			#This clears memory
require(rstan)     # 'http://mc-stan.org/' --> instructions how to install RStan
library(parallel)  # for parallel computing. Install the package first if not installed yet --> 'install.package("parallel")'
library(mail)      # to send emails. Install the package first if not installed yet --> 'install.package("mail")'
library(modeest)   # mode estimation. Install the package first if not installed yet --> 'install.package("modeest")'
set_cppo('fast')   # for best running speed

#################    Variables to customize for each user     #################
#-----------------------------------------------------------------------------#
saveDir = "~/Documents/myDirectory/subMyDirectory/PVL_delta/" # working directory where all codes should exist 
computeWAIC = T       # compute WAIC after run? T (true) or F (false)
parallelComputing = T  # T or F. If T, make sure # of cpus >= nchains
saveFiles = T         # save files? T or F
plotFigs = F           # plot figures? T or F
numPars = 4            # number of parameters    
nchains =  3 # number of chains
nwarmup = 1000 # warm up (discarded)
niter =  3000 # number of iterations (including warmup)
calcIndPars = "mean"  # summarize individual parameters as "mean", "median", or "mode"
payscale = 110 # payscale: divide the raw payoffs by this scale. default: 110 for Frontiers data --> median of immediate payoffs  = 0.5 & 1.0
inits = c(0.5, 0.5, 1.0, 1.0)  # inits for parameters (a, alpha, co, lambda)
maxTrials = 100 # maximum number of trials
POI = c("mu_A", "mu_alpha", "mu_cons", "mu_lambda",
        "sd_A", "sd_alpha", "sd_cons", "sd_lambda",
        "A", "alpha", "cons", "lambda" )   # parameters of interest. NA if estimating all parameters.
modelName = "PVL_delta"    #"3par_PT_softmax" or "3par_PT_DFT"
saveName = "IGTdata_healthy_control"  # data file (without .txt extension). It should be a '*.txt* file. 
dataHasHeader = T  # does the data file contain a header (column names)? T or F
colNames = c("trial", "deck", "gain", "loss", "tally", "borrow", "RT", "subjID") # columns of the data file.
sendEmail = TRUE
myEmail = "your_Email@domain.com"  # email to send the notification when the code is finished. 
#-----------------------------------------------------------------------------#

setwd( saveDir )                # go to save directory 
startTime=Sys.time()            #store the time

## load data
rawdata = read.table( paste( saveName, ".txt", sep=""), header = dataHasHeader )
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

  real<lower=0.01, upper=1.5> sd_A;
  real<lower=0.01, upper=1.5> sd_alpha;
  real<lower=0.01, upper=1.5> sd_cons;
  real<lower=0.01, upper=1.5> sd_lambda;

  real A_pr[N];
  real alpha_pr[N];
  real cons_pr[N];
  real lambda_pr[N];
 
}
transformed parameters {
  real<lower=0, upper=1> A[N]; 
  real<lower=0, upper=2> alpha[N]; 
  real<lower=0, upper=5> cons[N];   
  real<lower=0, upper=10> lambda[N];  

  for (i in 1:N) {
    A[i] <- Phi_approx( mu_A_pr + sd_A * A_pr[i] );
    alpha[i] <- 2 * Phi_approx( mu_alpha_pr + sd_alpha * alpha_pr[i] );
    cons[i] <- 5 * Phi_approx( mu_cons_pr + sd_cons * cons_pr[i] );
    lambda[i] <- 10 * Phi_approx( mu_lambda_pr + sd_lambda * lambda_pr[i] );
  }
}
model {
  mu_A_pr ~ normal(0, 1);
  mu_alpha_pr ~ normal(0, 1);
  mu_cons_pr ~ normal(0, 1);
  mu_lambda_pr ~ normal(0, 1);

  sd_A ~ uniform(0.01, 1.5);
  sd_alpha ~ uniform(0.01, 1.5);
  sd_cons ~ uniform(0.01, 1.5);
  sd_lambda ~ uniform(0.01, 1.5);

  for (i in 1:N) {
    real ev[4];    
    vector[4] p_next;
    vector[4] str;
    
    real curUtil;     # utility of curFb
    real const_util;  # const of util func
    real selected;    # selected deck
    real theta;       # theta = 3^c - 1

    # Matt trick
    A_pr[i] ~ normal(0, 1.0);   
    alpha_pr[i] ~ normal(0, 1.0);   
    cons_pr[i] ~ normal(0, 1.0);   
    lambda_pr[i] ~ normal(0, 1.0); 
    
    theta <- pow(3, cons[i]) -1;

    for (r in 1:4) {
      ev[r] <- 0;   # initial ev values
    }
    for (t in 1:(Tsubj[i]-1)) {
      if ( rewlos[i,t] >= 0) {  # x(t) >= 0
        curUtil <- pow(rewlos[i,t], alpha[i]);   
      } else {                  # x(t) < 0
        curUtil <- -1 * lambda[i] * pow( -1*rewlos[i,t], alpha[i]);
      }
      
      ev[ ydata[i, t] ] <- ev[ ydata[i, t] ] + A[i] * (curUtil - ev[ ydata[i, t] ] );      

      for (d in 1:4) {
        str[d] <- exp( theta * ev[d] );
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
  
  mu_A <- Phi_approx(mu_A_pr);
  mu_alpha <- Phi_approx(mu_alpha_pr) * 2;
  mu_cons <- Phi_approx(mu_cons_pr) * 5; 
  mu_lambda <- Phi_approx(mu_lambda_pr) * 10;  

}
' 

################################################################################
# THE DATA.  ###################################################################
################################################################################
#numPars = 4

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
    mu_alpha_pr = qnorm(inits[2] /2),
    mu_cons_pr = qnorm( inits[3] /5 ),
    mu_lambda_pr = qnorm( inits[4] / 10 ),
    sd_A = 1, 
    sd_alpha = 1,
    sd_cons = 1,
    sd_lambda = 1,
    A_pr = rep( qnorm(inits[1]), numSubjs),
    alpha_pr = rep( qnorm(inits[2]/2), numSubjs),
    cons_pr = rep( qnorm(inits[3]/5), numSubjs),
    lambda_pr = rep( qnorm(inits[4]/10), numSubjs)
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

sd_A = parVals$sd_A
sd_alpha = parVals$sd_alpha
sd_cons = parVals$sd_cons
sd_lambda = parVals$sd_lambda

A = parVals$A
alpha = parVals$alpha
cons = parVals$cons
lambda = parVals$lambda

# Individual parameters (e.g., individual posterior means)
allIndPars = array(NA, c(numSubjs, numPars))
allIndPars = as.data.frame(allIndPars)
for (i in 1:numSubjs) {
  if (calcIndPars=="mean") {
    allIndPars[i, ] = c( mean(A[, i]), mean(alpha[, i]), mean(cons[, i]), mean(lambda[, i]) )    
  } else if (calcIndPars=="median") {
    allIndPars[i, ] = c( median(A[, i]), median(alpha[, i]), median(cons[, i]), median(lambda[, i]) )    
  } else if (calcIndPars=="mode") {
    allIndPars[i, ] = c( mlv(A[, i], method="shorth")[1], 
                         mlv(alpha[, i], method="shorth")[1],
                         mlv(cons[, i], method="shorth")[1],
                         mlv(lambda[, i], method="shorth")[1] )
  }   
}  
allIndPars = cbind(allIndPars, subjList)
colnames(allIndPars) = c("A", "alpha", "c", "lambda", "subjID")  


# save this workspace for just in case data would be lost in a later section.
if (saveFiles) {
  save.image( file.path(saveDir, paste("IGT_", modelName, "_results_", saveName, 
                                       "_numSteps", niter, "_numChains", nchains,
                                       ".Rdata", sep="") )  )
}

if (plotFigs) {
  quartz()
  traceplot(fit)  # traceplot

  # Plot hyper-parameters
  x11(width=8, height=3)
  layout( matrix(1:numPars, 1, numPars, byrow=F))
  hist(aHypSample, main=round(mean(aHypSample),3), freq=F, xlab="A" )#, xlim=c(0, 1.5))
  hist(alphaHypSample, main=round(mean(alphaHypSample),3), freq=F, xlab=expression(alpha) )#, xlim=c(0, 5))
  hist(cHypSample, main=round(mean(cHypSample),3), freq=F, xlab=expression(c) )#, xlim=c(0, 5))
  hist(lambdaHypSample, main=round(mean(lambdaHypSample),3), freq=F, xlab=expression(lambda))#, xlim=c(0, 5))
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
  source("WAIC_PVL_delta_cleaned.R")
}


# end of the code


