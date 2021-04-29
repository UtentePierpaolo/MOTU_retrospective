
# fdrrs

# functions for calculating incidence rates and their confidence intervals


fdrr <- function(gmem, namesx=NULL){

  vc <- fixef(gmem)

  # vc <- vc["(Intercept)"] + c(0, vc[2:nrow(a$coefficients)])
  vsted <- summary(gmem)$coefficients[,"Std. Error"]
  ndays <- 1000
  irr <- ndays*exp(vc)
  irr_u <- ndays*exp(vc+ vsted)
  irr_l <- ndays*exp(vc - vsted)
  if (is.null(namesx)){
    namesx <- levels(gmem@frame$KneeCategory)
  }
  data.estimates <- data.frame(irr=irr, irr_u=irr_u, irr_l=irr_l, KneeCategory=namesx)
  return(data.estimates)
}


fdrr_mp <- function(gmem, newdata, CIlevel){
  
  # "mp" for "marginal predictions"
  # see e.g.
  # M. Pavlou, G. Ambler, S. Seaman, and R. Z. Omar, 
  # "A note on obtaining correct marginal predictions from a random intercepts model for binary outcomes," BMC Med. Res. Methodol., vol. 15, no. 1, p. 59, Aug. 2015, doi: 10.1186/s12874-015-0046-6.
  
  require(lme4)
  
  vc <- VarCorr(gmem)
  sigmau <- attr(vc$AnonymousID,"stddev")
  u <- qnorm(seq(0.001, 0.999, 0.001))*sigmau
  
  predict.fun <- function(my.poisson_glmerMod) {
    # fixed-effect term, linear scale
    fet <- predict(my.poisson_glmerMod, newdata = newdata, re.form = NA)   # This is predict.merMod
    # equivalent to: log(1000) + fixef(my.poisson_glmerMod)...
    mfu <- matrix(nrow=length(fet),ncol=length(u))
    for(indr in 1:length(fet)){
      mfu[indr,] <- fet[indr]+u
    }
    rowMeans(exp(mfu))
  }
  
  ir <- predict.fun(gmem)
  
  # calculate CI
  poim.boots <- bootMer(gmem, predict.fun, nsim = 100) # parallel="multicore", ncpus=3
  mCI <-  confint(poim.boots, level=CIlevel)
  
  data.estimates <- data.frame(irr=ir, irr_l=mCI[,1], irr_u=mCI[,2], KneeCategory=newdata$KneeCategory)
  return(data.estimates)
}


# fdrr_svy <- function(gmem, namesx=NULL){
#   
#   vc <- coef(gmem)
#   vc <- vc[paste0("KneeCategory",vkc)]
#   
#   # vc <- vc["(Intercept)"] + c(0, vc[2:nrow(a$coefficients)])
#   vsted <- summary(gmem)$coefficients[,"Std. Error"]
#   vsted <- vsted[paste0("KneeCategory",vkc)]
#   
#   ndays <- 1000
#   irr <- ndays*exp(vc)
#   irr_u <- ndays*exp(vc+ vsted)
#   irr_l <- ndays*exp(vc - vsted)
#   if (is.null(namesx)){
#     namesx <- vkc
#   }
#   data.estimates <- data.frame(irr=irr, irr_u=irr_u, irr_l=irr_l, KneeCategory=namesx)
#   return(data.estimates)
# }


fdrr_svy_pred <- function(ates0m, newdata, ndays, CIlevel){
  
  require(survey)
  
  # newdata <- data.frame(LengthOfStay=1, KneeCategory=factor(c("LK","AMK","FK","MPK")))
  plink <- predict(ates0m, newdata=newdata)
  ir <- exp(coef(plink) + log(ndays))
  irci <- exp(confint(plink, level=CIlevel) + log(ndays))
  data.estimates <- data.frame(irr=ir, irr_l=irci[,1], irr_u=irci[,2], KneeCategory=newdata$KneeCategory)
  return(data.estimates)
}
