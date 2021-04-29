# models_ExposureOutcome


# models for the association between KneeCategory (exposure, z) and FallsWithProsthesis (outcome, y)

# 1. model fitting
# 2. inference: on KneeCategory + pairwise post-hoc comparisons
# 3. prediction

# for unweighted and PS-weighted models


models_ExposureOutcome <- function(dfm){
  
  require(lme4)
  require(survey)
  source("fdrrs.R")
  require(car)
  
  nmin = 5 # minimum number of falls to calculate falls rate in a group
  
  
  # models for unadjusted fall rates----------------------
  
  # voutcome <- c("NumberAnyFall","NumberFallsWithProsthesis","NumberFallsWithoutProsthesis")
  voutcome <- "NumberFallsWithProsthesis"
  vformula_all <- paste(voutcome, " ~ offset(log(LengthOfStay)) + (1|AnonymousID)")
  vformula0 <- paste(voutcome, " ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID)")
  vformula1 <- paste(voutcome, " ~ offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID)")
  vaformula <- paste(voutcome, " ~ KneeCategory")
  lmodel_all <- list()
  lmodel0 <- list()
  lmodel1 <- list()
  lanova <- list()
  for (ind in 1:length(voutcome)){
    # model on all HS
    lmodel_all[[ind]] <- glmer(vformula_all[ind], data=dfm, family=poisson(link="log"), nAGQ=20)
    
    # if on some knee category there are less than nmin(=5) falls, exclude that category
    adfm <- aggregate(as.formula(vaformula[ind]), data=dfm, FUN=sum)
    # knee category to exclude from the analyses
    kcte <- adfm[adfm[,voutcome[ind]] < nmin, "KneeCategory"]
    cdfm <- droplevels(dfm[!(dfm$KneeCategory %in% kcte),])
    
    lmodel0[[ind]] <- glmer(vformula0[ind], data=cdfm, family = poisson(link = "log"), nAGQ = 20)
    lmodel1[[ind]] <- glmer(vformula1[ind], data=cdfm, family = poisson(link = "log"), nAGQ = 20)
    
    # inference on KneeCategory, PS-weighted model
    lanova[[ind]] <- Anova(lmodel1[[ind]])
  }
  names(lmodel_all) <- voutcome
  names(lmodel0) <- voutcome
  names(lmodel1) <- voutcome
  names(lanova) <- voutcome
  
  
  # paiwise comparisons
  wht <- glht(modelszy$lmodel1$NumberFallsWithProsthesis, 
              linfct = mcp(KneeCategory = c("AMK - LK = 0", "FK - LK = 0", "MPK - LK = 0", "FK - AMK = 0", "AMK - MPK  = 0", "FK - MPK = 0") ))
  
  
  
  # models for fall rates, weighted with propensity score (PS)----------------------
  # ATE Estimates.
  
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
  
  # inference on KneeCategory, PS-weighted model
  anova_ates <- anova(ates, ates_null, method="Wald")

  # FK - AMK
  ate_FK_AMK <- data.frame(svycontrast(ates, quote(FK - AMK)))
  names(ate_FK_AMK) <- c("Estimate", "Std. Error")
  ate_FK_AMK$"t value" <- ate_FK_AMK$Estimate/ate_FK_AMK$"Std. Error"
  ate_FK_AMK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_FK_AMK$"t value"),
                                   df=summary(ates)$df.residual))
  # AMK - MPK
  ate_AMK_MPK <- data.frame(svycontrast(ates, quote(AMK - MPK)))
  names(ate_AMK_MPK) <- c("Estimate", "Std. Error")
  ate_AMK_MPK$"t value" <- ate_AMK_MPK$Estimate/ate_AMK_MPK$"Std. Error"
  ate_AMK_MPK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_AMK_MPK$"t value"),
                                   df=summary(ates)$df.residual))
  
  # FK - MPK
  ate_FK_MPK <- data.frame(svycontrast(ates, quote(FK - MPK)))
  names(ate_FK_MPK) <- c("Estimate", "Std. Error")
  ate_FK_MPK$"t value" <- ate_FK_MPK$Estimate/ate_FK_MPK$"Std. Error"
  ate_FK_MPK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_FK_MPK$"t value"),
                                    df=summary(ates)$df.residual))
  
  # all contrasts
  ates_allc <- rbind(summary(ates)$coef[-1,], ate_FK_AMK, ate_AMK_MPK, ate_FK_MPK)
  rownames(ates_allc) <- c("AMK vs. LK",
                          "FK vs. LK", "MPK vs. LK", "FK vs. AMK",
                          "AMK vs. MPK", "FK vs MPK")

  # ## Estimate the means. ##
  # ## The intercept estimates the mean for LK ##
  # com_mean <- summary(ates)$coef[1,]
  # 
  # pop_means <- data.frame(svycontrast(ates,list(AMK=c(1,1,0,0), FK=c(1,0,1,0), MPK=c(1,0,0,1))))
  # names(pop_means) <- c("Estimate", "Std. Error")
  # pop_means$"t value" <- pop_means$Estimate/pop_means$"Std. Error"
  # pop_means$"Pr(>|t|)" <- 2*(1-pt(abs(pop_means$"t value"),
  #                                 df=summary(ates)$df.residual))
  # ## Combine all three means. ##
  # pop_means <- rbind(LK=com_mean, pop_means)
  # pop_means
  # 
  # wht <- glht(lmodel1[["NumberFallsWithProsthesis"]], linfct = mcp(KneeCategory = "Tukey"))
  # summary(wht)
  # 
  # ates2 <- svyglm(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory, 
  #                 design=dsd, family=quasipoisson())
  # 
  ates0 <- svyglm(NumberFallsWithProsthesis ~ -1 + offset(log(LengthOfStay)) + KneeCategory,
                   design=dsd, family=quasipoisson())
  # summary(ates0) # ok, as pop_means
  # 
  
  # wht <- glht(ates0, linfct = mcp(KneeCategory = "Tukey"))
  # summary(wht)
  # !! compare using glht and script with svycontrast... !!
  
  
  # Prediction
  lRR[["NumberFallsWithProsthesis_PS"]] <- fdrr_svy(gmem=ates0)
  
  
  # Incidence for IRR for each knee category and their 95% CI----------------------------
  lRR <- list()
  # lRR[["NumberAnyFall"]] <- fdrr(gmem=lmodel0[["NumberAnyFall"]])
  # lRR[["NumberFallsWithProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithProsthesis"]])
  # lRR[["NumberFallsWithoutProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithoutProsthesis"]])
  newdata_kc <- data.frame(LengthOfStay=rep(1000,4), KneeCategory=factor(c("LK","AMK","FK","MPK")))
  lRR[["NumberFallsWithProsthesis"]] <- fdrr_mp(gmem=lmodel0[["NumberFallsWithProsthesis"]],
                                                newdata = newdata_kc, CIlevel = 0.95)
  
  
  lIR_all <- list()
  newdata_all <- data.frame(LengthOfStay=1000, KneeCategory="All")
  lIR_all[["NumberFallsWithProsthesis"]] <- fdrr_mp(gmem=lmodel_all[["NumberFallsWithProsthesis"]],
                                                    newdata = newdata_all, CIlevel=.95)
  
  return(list(lmodel_all=lmodel_all, lmodel0=lmodel0, lmodel1=lmodel1, # unweighted models
              ates0=ates0, ates=ates, ates_allc=ates_allc, # PS-weighted model
              lanova=lanova, anova_ates=anova_ates, 
              lRR=lRR))
}



