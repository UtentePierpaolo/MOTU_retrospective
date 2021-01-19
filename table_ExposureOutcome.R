# table_ExposureOutcome


# table with statistics on KneeCategory
# and associations with falls occurred with prosthesis

# number HS, patients, hospitals days, falls
# p-value for (KneeCategory vs NumberFallsWithProsthesis)


# !! Some problems, to be completed !!

# see IncidenceRates_trials.R

table_ExposureOutcome <- function(dfm, modelszy){


  require(lme4)
  require(survey)
  library("Hmisc")
  
  nmin = 5 # minimum number of falls to calculate falls rate in a group

  # pre-processing-------------------------

  # unpackage parameters
  lmodel_all <- modelszy[["lmodel_all"]]
  lmodel0 <- modelszy[["lmodel0"]]
  lmodel1 <- modelszy[["lmodel1"]]
  ates0 <- modelszy[["ates0"]]
  lanova <- modelszy[["lanova"]]
  lRR <- modelszy[["lRR"]]
          
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


    # include in table dfskc the model-estimated fall rates and their 95% CI
  for (coutcome in names(lRR)){
    dfskc <- merge(dfskc, lRR[[coutcome]], by="KneeCategory", all.x=T)
    names(dfskc)[names(dfskc)=="irr"] <- paste0("IR_",coutcome)
    dfskc[,paste0("IR_",coutcome,"_CI")] <- paste0(round(dfskc$irr_l, digits=2), "-", round(dfskc$irr_u, digits=2))
    names(dfskc)[names(dfskc)=="irr_u"] <- paste0("IR_",coutcome,"_u")
    names(dfskc)[names(dfskc)=="irr_l"] <- paste0("IR_",coutcome,"_l")
  }

  dfskc$KneeCategory <- as.character(dfskc$KneeCategory)
  dfskc[nrow(dfskc)+1, "KneeCategory"] <- "All"
  for (coutcome in names(lmodel_all)){
    RR_all_cout <- fdrr(gmem=lmodel_all[[coutcome]], namesx="All")
    dfskc[dfskc$KneeCategory=="All", paste0("IR_", coutcome)] <- RR_all_cout$irr
    dfskc[dfskc$KneeCategory=="All", paste0("IR_",coutcome,"_CI")] <- paste0(round(RR_all_cout$irr_l,digit=2), "-", round(RR_all_cout$irr_u,digit=2))
  }

  # sort
  row.names(dfskc) <- dfskc$KneeCategory
  dfskc <- dfskc[c("LK","AMK","FK","MPK","NA","All"),]

  
  return(dfskc)
}

