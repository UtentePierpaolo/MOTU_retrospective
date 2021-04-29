# ExposureOutcome_inference

# Exposure: KneeCategory (z)
# Outcome: FallsWithProsthesis (y)

# 1. inference on the effect of KneeCategory
# 2. post-hoc pairwise comparisons between knee categories (LK, AMK, FK, MPK)

# for both the unweighted and the PS-weighted case


ExposureOutcome_inference <- function(lmodels){
  
  
  require(lme4)
  require(survey)
  require(car)
  require(multcomp)
  
  # unweighted case----------------
  
  # inference on KneeCategory, PS-weighted model
  resanova_unw <- Anova(lmodels$lmodels_unw$model1)
  
  # pairwise comparisons
  wht <- glht(lmodels$lmodels_unw$model1, 
              linfct = mcp(KneeCategory = c("AMK - LK = 0", "FK - LK = 0", "MPK - LK = 0", "FK - AMK = 0", "AMK - MPK  = 0", "FK - MPK = 0") ))
  pcomp_unw <- confint(wht)
  
  
  # PS-weighted case-------------------------------
  
  ates <- lmodels$lmodels_w$model1
  
  # inference on KneeCategory, PS-weighted model
  resanova_w <- anova(ates, lmodels$lmodels_w$model_all, method="Wald")
  
  # pairwise comparisons
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
  
  lcontrasts <- list()
  mci <- ates_allc #Estimate        lwr        upr
  mci$lwr <- qt(p=0.025, df=summary(lmodels$lmodels_w$model1)$df.residual)*ates_allc$`Std. Error` + ates_allc$Estimate
  mci$upr <- qt(p=0.975, df=summary(lmodels$lmodels_w$model1)$df.residual)*ates_allc$`Std. Error` + ates_allc$Estimate
  lcontrasts$confint <- as.matrix(mci)
  pcomp_w <- lcontrasts
  
  return(list(resanova_unw=resanova_unw, pcomp_unw=pcomp_unw, 
              resanova_w=resanova_w, pcomp_w=pcomp_w))
}