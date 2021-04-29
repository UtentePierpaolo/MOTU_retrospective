# ExposureOutcome_modelFitting


# model fitting for the association between KneeCategory (exposure, z) and FallsWithProsthesis (outcome, y)

# unweighted and PS-weighted models


ExposureOutcome_modelFitting <- function(dfm){
  
  
  require(lme4)
  require(survey)
  
  nmin = 5 # minimum number of falls to calculate falls rate in a group
  

  # unweighted models-----------------------
  
  voutcome <- "NumberFallsWithProsthesis"
  vformula_all <- paste(voutcome, " ~ offset(log(LengthOfStay)) + (1|AnonymousID)")
  vformula0 <- paste(voutcome, " ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID)")
  vformula1 <- paste(voutcome, " ~ offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID)")
  vaformula <- paste(voutcome, " ~ KneeCategory")
  
  # model on all hospitalizations
  model_all <- glmer(vformula_all, data=dfm, family=poisson(link="log"), nAGQ=20)
  
  # if on some knee category there are less than nmin(=5) falls, exclude that category
  adfm <- aggregate(as.formula(vaformula), data=dfm, FUN=sum)
  # knee category to exclude from the analyses
  kcte <- adfm[adfm[,voutcome] < nmin, "KneeCategory"]
  cdfm <- droplevels(dfm[!(dfm$KneeCategory %in% kcte),])
  
  model0 <- glmer(vformula0, data=cdfm, family = poisson(link = "log"), nAGQ = 20)
  model1 <- glmer(vformula1, data=cdfm, family = poisson(link = "log"), nAGQ = 20)
  
  lmodels_unw <- list(model_all=model_all, model0=model0, model1=model1)
  

  # PS-weighted models--------------------------
  
  # if on some knee category there are less than nmin(=5) falls, exclude that category
  adfm <- aggregate(as.formula("NumberFallsWithProsthesis  ~ KneeCategory"), data=dfm, FUN=sum)
  # knee category to exclude from the analyses
  kcte <- adfm[adfm[,"NumberFallsWithProsthesis"] < nmin, "KneeCategory"]
  cdfm <- droplevels(dfm[!(dfm$KneeCategory %in% kcte),])
  
  # drop where there is no information on knee category
  cdfm <- cdfm[!is.na(cdfm$KneeCategory),]
  
  cdfm$LK <- (cdfm$KneeCategory %in% "LK")*1
  cdfm$AMK <- (cdfm$KneeCategory %in% "AMK")*1
  cdfm$FK <- (cdfm$KneeCategory %in% "FK")*1
  cdfm$MPK <- (cdfm$KneeCategory %in% "MPK")*1
  
  dsd <- svydesign(id= ~ 1, weights= ~ w, data=cdfm)
  ates <- svyglm(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + AMK+FK+MPK,
                 design=dsd, family=quasipoisson())
  ates_null <- svyglm(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)),
                      design=dsd, family=quasipoisson())
  ates0 <- svyglm(NumberFallsWithProsthesis ~ -1 + offset(log(LengthOfStay)) + KneeCategory,
                  design=dsd, family=quasipoisson())
  
  lmodels_w <- list(model_all=ates_null, model0=ates0, model1=ates)
  
  return(list(lmodels_unw=lmodels_unw, lmodels_w=lmodels_w))

}