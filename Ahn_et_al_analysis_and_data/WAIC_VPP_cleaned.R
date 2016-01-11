# WAIC_VPP.R
# RStan output data should be loaded first. 
# Computing WAIC of the Value-Plus-Perseverance Model (Worthy et al. 2014, Frontiers in Psychology)
# Programmed by Woo-Young Ahn (www.ahnlab.org)

lppd = vector("numeric", numSubjs); #initialization. 
pWAIC1 = vector("numeric", numSubjs); #initialization.  pWAIC1 = pWAIC1_part1 + pWAIC_part2
pWAIC1_part1 = vector("numeric", numSubjs); #initialization. pWAIC1_part1 = 2*lppd
pWAIC1_part2 = vector("numeric", numSubjs); #initialization. 
pWAIC2 = vector("numeric", numSubjs); #initialization.  pWAIC2
WAIC = vector("numeric", numSubjs); #initialization. 

numSample_orig = dim(alpha)[1]    # orig number of MCMC samples
numSample = 1000  # Random samples for computing WAIC
selectedSeq = sample(1:numSample_orig, numSample);
usePWAIC2 = 1   # use pWAIC2 as recommended? 1 or 0 (default=1)

cat("# of iterations = ", numSample, "\n")

for (subjIdx in 1:numSubjs) {
  
  LLperSubj = vector("numeric", numSample)  # for LL for each subject. OLD
  probPerSubj = array(NA, c(numSample, maxTrials))
  tmpDat = subset( rawdata, subjID == subjList[subjIdx] )
  
  for (sIdx in 1:numSample) {  
    tmpA   = A[selectedSeq[sIdx], subjIdx]
    tmpAlpha = alpha[selectedSeq[sIdx], subjIdx] 
    tmpCons  = cons[selectedSeq[sIdx], subjIdx]
    tmpLambda  = lambda[selectedSeq[sIdx], subjIdx]
    tmpEpP   = epP[selectedSeq[sIdx], subjIdx];
    tmpEpN   = epN[selectedSeq[sIdx], subjIdx];
    tmpK   = K[selectedSeq[sIdx], subjIdx];
    tmpW   = w[selectedSeq[sIdx], subjIdx];
    
    deck = tmpDat$deck
    gain = tmpDat$gain / payscale
    loss = tmpDat$loss / payscale
    
    ev = vector("numeric", 4)  # ev
    str = vector("numeric", 4)  # strength
    pCurr = c(0.25, 0.25, 0.25, 0.25)  # p[t+1]
    pers = vector("numeric", 4)  # persistence
    
    for( t in 1:Tsubj[subjIdx]) {   # t: trial number
      # start a trial with computing pCurr from ev --> str. 
      # pCurr: prob of choosing the chosen deck on the current trial
      str = exp( ( tmpW*ev + (1-tmpW)*pers )*(3^tmpCons - 1) )    # str[d] <- exp( theta * (w[i]*ev[d]+(1-w[i])*pers[d]) );
      sumStr = sum(str)
      pCurr = str / sumStr
      currCh = deck[t]   # current choice on trial t
      
      LLperSubj[sIdx] = LLperSubj[sIdx] + log( pCurr[ currCh ] ) #log likelihood
      probPerSubj[sIdx, t] = pCurr[currCh]
      
      # decay perseverance --> decayed
      pers = pers * tmpK
      
      # obj feedback --> subj feedback
      currFb = gain[t] - abs(loss[t])
      if ( currFb >= 0) {  # x(t) >= 0
        currUtil = (abs(currFb))^tmpAlpha
        pers[ currCh ] <- pers[ currCh ] + tmpEpP;  # perseverance term (+)
      } else {             # x(t) < 0
        currUtil = -tmpLambda*(abs(currFb))^tmpAlpha
        pers[ currCh ] <- pers[ currCh ] + tmpEpN;  # perseverance term (+)
      }
      
      # update ev w/ delta
      ev[currCh] = ev[currCh] + tmpA*(currUtil - ev[currCh])    
      
    } # end of t (trial from 1 to T) for loop
    
  } # end of sIdx (simulation agents) for loop 
  
  lppd[subjIdx] = sum( log( apply(probPerSubj, 2, mean) ), na.rm=T )
  
  pWAIC1_part1[subjIdx] = 2* lppd[subjIdx]
  pWAIC1_part2_tmp = apply( log(probPerSubj), 2, mean)
  pWAIC1_part2[subjIdx] = -2 * sum( pWAIC1_part2_tmp, na.rm=T )
  pWAIC1[subjIdx] = pWAIC1_part1[subjIdx] + pWAIC1_part2[subjIdx] # pWAIC1
  
  pWAIC2_tmp = log(probPerSubj)
  pWAIC2[subjIdx] = sum( apply(pWAIC2_tmp, 2, var), na.rm=T)  # pWAIC2
  
  if (usePWAIC2) {
    WAIC[subjIdx] = -2 * ( lppd[subjIdx] - pWAIC2[subjIdx] ) 
  } else {
    WAIC[subjIdx] = -2 * ( lppd[subjIdx] - pWAIC1[subjIdx] ) 
  }
  
  cat("Subject ", subjIdx, " out of ", numSubjs, " done \n")  
} # end of subjIdx for loop    

# print WAIC
WAIC_overall = sum(WAIC)    
cat("data name = ", saveName, "\n")
cat("WAIC=", WAIC_overall, "\n")
#hist(WAIC, main="Histogram of WAIC over all subjects")

# end of the code
