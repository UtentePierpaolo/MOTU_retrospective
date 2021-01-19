# models_ExposureOutcome_NegBin


# models for the association between 
# KneeCategory (exposure, z)
# and FallsWithProsthesis (outcome, y)


models_ExposureOutcome_NegBin <- function(dfm){
  
  require(lme4)
  source("fdrrs.R")
  
  nmin = 5 # minimum number of falls to calculate falls rate in a group
  
  # models for unadjusted fall rates----------------------
  
  voutcome <- "NumberFallsWithProsthesis"
  vformula0 <- paste(voutcome, " ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID)")
  vaformula <- paste(voutcome, " ~ KneeCategory")
  lmodel0 <- list()
  
  for (ind in 1:length(voutcome)){

    # if on some knee category there are less than nmin(=5) falls, exclude that category
    adfm <- aggregate(as.formula(vaformula[ind]), data=dfm, FUN=sum)
    # knee category to exclude from the analyses
    kcte <- adfm[adfm[,voutcome[ind]] < nmin, "KneeCategory"]
    cdfm <- droplevels(dfm[!(dfm$KneeCategory %in% kcte),])
    
    lmodel0[[ind]] <- glmer.nb(vformula0[ind], data=cdfm, nAGQ = 20) #
  }
  names(lmodel0) <- voutcome

  # Incidence for IRR for each knee category and their 95% CI----------------------------
  lRR <- list()
  lRR[["NumberFallsWithProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithProsthesis"]])

  return(list(lmodel0=lmodel0, lRR=lRR))
}

