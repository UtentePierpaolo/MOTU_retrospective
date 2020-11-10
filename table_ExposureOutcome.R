# table_ExposureOutcome


# table with statistics on KneeCategory
# and associations with falls occurred with prosthesis

# number HS, patients, hospitals days, falls
# p-value for (KneeCategory vs NumberFallsWithProsthesis)


# !! Some problems, to be completed !!


table_ExposureOutcome <- function(dfm){


  require(lme4)
  require(survey)
  
  nmin = 5 # minimum number of falls to calculate falls rate in a group

  # pre-processing-------------------------

  # treat NA on KneeCategory as a group
  ckc <- levels(dfm$KneeCategory)
  dfm$KneeCategory <- factor(dfm$KneeCategory, exclude = "")
  ckc <- c(ckc, "NA")

  dfm$one <- 1


  # create table with statistics---------------

  dfskc <- data.frame( # number of HS for each knee category
    KneeCategory = ckc,
    NumberHospitalStays = aggregate(one  ~ KneeCategory, data=dfm, FUN=sum)[,"one"],
    # number of different patients for each knee category
    NumberPatients = aggregate(one  ~ KneeCategory, data=unique(dfm[,c("AnonymousID","KneeCategory","one")]), FUN=sum)[,"one"],
    # number of hospital days for each knee category
    NumberHospitalDays= aggregate(LengthOfStay ~ KneeCategory, data=dfm, FUN=sum)[,"LengthOfStay"],
    # falls
    NumberAnyFall = aggregate(NumberAnyFall ~ KneeCategory, data=dfm, FUN=sum)[,"NumberAnyFall"],
    NumberFallsWithProsthesis = aggregate(NumberFallsWithProsthesis ~ KneeCategory, data=dfm, FUN=sum)[,"NumberFallsWithProsthesis"],
    NumberFallsWithoutProsthesis = aggregate(NumberFallsWithoutProsthesis ~ KneeCategory, data=dfm, FUN=sum)[,"NumberFallsWithoutProsthesis"])

  row.names(dfskc) <- dfskc$KneeCategory

  # stop considering NA as a KneeCategory
  dfm$KneeCategory <- factor(dfm$KneeCategory, exclude =NA)

  # fall rates----------------------

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

  ###############################################################################################
  
  ## ATE Estimates.
  
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
  
  ate_FK_AMK <- data.frame(svycontrast(ates, quote(FK - AMK)))
  names(ate_FK_AMK) <- c("Estimate", "Std. Error")
  ate_FK_AMK$"t value" <- ate_FK_AMK$Estimate/ate_FK_AMK$"Std. Error"
  ate_FK_AMK$"Pr(>|t|)" <- 2*(1-pt(abs(ate_FK_AMK$"t value"),
                                    df=summary(ates)$df.residual))
  
  ates_all <- rbind(summary(ates)$coef[-1,], ate_FK_AMK)
  rownames(ates_all) <- c("AMK vs. LK",
                          "FK vs. LK", "MPK vs. LK", "FK vs. AMK")
  
  ## Estimate the means. ##
  ## The intercept estimates the mean for LK ##
  com_mean <- summary(ates)$coef[1,]
  
  pop_means <- data.frame(svycontrast(ates,list(AMK=c(1,1,0,0), FK=c(1,0,1,0), MPK=c(1,0,0,1))))
  names(pop_means) <- c("Estimate", "Std. Error")
  pop_means$"t value" <- pop_means$Estimate/pop_means$"Std. Error"
  pop_means$"Pr(>|t|)" <- 2*(1-pt(abs(pop_means$"t value"),
                                  df=summary(ates)$df.residual))
  ## Combine all three means. ##
  pop_means <- rbind(LK=com_mean, pop_means)
  pop_means
  
  wht <- glht(lmodel1[["NumberFallsWithProsthesis"]], linfct = mcp(KneeCategory = "Tukey"))
  summary(wht)
  
  ates2 <- svyglm(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory, 
                                design=dsd, family=quasipoisson())
  
  glht(ates, linfct=...)
  
  fcg(confint(wht), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
  
  
  ####################################################################################
  
  
  
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

  
  lRR <- list()
  lRR[["NumberAnyFall"]] <- fdrr(gmem=lmodel0[["NumberAnyFall"]])
  lRR[["NumberFallsWithProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithProsthesis"]])
  lRR[["NumberFallsWithoutProsthesis"]] <- fdrr(gmem=lmodel0[["NumberFallsWithoutProsthesis"]])

  # include in table dfskc the model-estimated fall rates and their 95% CI
  for (coutcome in voutcome){
    dfskc <- merge(dfskc, lRR[[coutcome]], by="KneeCategory", all.x=T)
    names(dfskc)[names(dfskc)=="irr"] <- paste0("IR_",coutcome)
    dfskc[,paste0("IR_",coutcome,"_CI")] <- paste0(round(dfskc$irr_l, digits=2), "-", round(dfskc$irr_u, digits=2))
    names(dfskc)[names(dfskc)=="irr_u"] <- paste0("IR_",coutcome,"_u")
    names(dfskc)[names(dfskc)=="irr_l"] <- paste0("IR_",coutcome,"_l")
  }

  dfskc$KneeCategory <- as.character(dfskc$KneeCategory)
  dfskc[nrow(dfskc)+1, "KneeCategory"] <- "All"
  for (coutcome in voutcome){
    RR_all_cout <- fdrr(gmem=lmodel_all[[coutcome]], namesx="All")
    dfskc[dfskc$KneeCategory=="All", paste0("IR_", coutcome)] <- RR_all_cout$irr
    dfskc[dfskc$KneeCategory=="All", paste0("IR_",coutcome,"_CI")] <- paste0(round(RR_all_cout$irr_l,digit=2), "-", round(RR_all_cout$irr_u,digit=2))
  }

  # sort
  row.names(dfskc) <- dfskc$KneeCategory
  dfskc <- dfskc[c("LK","AMK","FK","MPK","NA","All"),]

  
  return(list(tab=dfskc, lmodel1=lmodel1, lRR=lRR, lanova=lanova))
}

