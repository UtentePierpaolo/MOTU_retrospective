# models_ExposureOutcome


# models for the association between 
# KneeCategory (exposure, z)
# and FallsWithProsthesis (outcome, y)


models_ExposureOutcome <- function(dfm){
  
  require(lme4)
  require(survey)
  source("fdrrs.R")
  require(car)
  
  nmin = 5 # minimum number of falls to calculate falls rate in a group
  
  
  # models for unadjusted fall rates----------------------
  
  voutcome <- c("NumberAnyFall","NumberFallsWithProsthesis","NumberFallsWithoutProsthesis")
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
    lanova[[ind]] <- Anova(lmodel1[[ind]])
  }
  names(lmodel_all) <- voutcome
  names(lmodel0) <- voutcome
  names(lmodel1) <- voutcome
  names(lanova) <- voutcome
  
  
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

  # FK - AMK
  ate_FK_AMK <- data.frame(svycontrast(ates, quote(FK - AMK)))
  names(ate_FK_AMK) <- c("Estimate", "Std. Error")
  ate_FK_AMK$"t value" <- ate_FK_AMK$Estimate/ate_FK_AMK$"Std. Error"
  ate_FK_AMK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_FK_AMK$"t value"),
                                   df=summary(ates)$df.residual))
  # MPK - AMK
  ate_MPK_AMK <- data.frame(svycontrast(ates, quote(MPK - AMK)))
  names(ate_MPK_AMK) <- c("Estimate", "Std. Error")
  ate_MPK_AMK$"t value" <- ate_MPK_AMK$Estimate/ate_MPK_AMK$"Std. Error"
  ate_MPK_AMK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_MPK_AMK$"t value"),
                                   df=summary(ates)$df.residual))
  
  # MPK - FK
  ate_MPK_FK <- data.frame(svycontrast(ates, quote(MPK - FK)))
  names(ate_MPK_FK) <- c("Estimate", "Std. Error")
  ate_MPK_FK$"t value" <- ate_MPK_FK$Estimate/ate_MPK_FK$"Std. Error"
  ate_MPK_FK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_MPK_FK$"t value"),
                                    df=summary(ates)$df.residual))
  
  # all contrasts
  ates_allc <- rbind(summary(ates)$coef[-1,], ate_FK_AMK, ate_MPK_AMK, ate_MPK_FK)
  rownames(ates_allc) <- c("AMK vs. LK",
                          "FK vs. LK", "MPK vs. LK", "FK vs. AMK",
                          "MPK vs. AMK", "MPK vs FK")

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
  
  
  
  # Incidence for IRR for each knee category and their 95% CI----------------------------
  lRR <- list()
  lRR[["NumberAnyFall"]] <- fdrr(gmem=lmodel0[["NumberAnyFall"]])
  lRR[["NumberFallsWithProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithProsthesis"]])
  lRR[["NumberFallsWithoutProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithoutProsthesis"]])
  lRR[["NumberFallsWithProsthesis_PS"]] <- fdrr_svy(gmem=ates0)
  
  
  return(list(lmodel_all=lmodel_all, lmodel0=lmodel0, lmodel1=lmodel1, 
              ates0=ates0, ates=ates, ates_allc=ates_allc,
              lanova=lanova, lRR=lRR))
}



