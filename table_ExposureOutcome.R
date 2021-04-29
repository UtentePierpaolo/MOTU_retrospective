# table_ExposureOutcome


# table with statistics on KneeCategory
# and associations with falls occurred with prosthesis

# number HS, patients, hospitals days, falls
# p-value for (KneeCategory vs NumberFallsWithProsthesis)


# !! Some problems, to be completed !!

# see IncidenceRates_trials.R

table_ExposureOutcome <- function(dfm, lIR){


  # require(lme4)
  # require(survey)
  library("Hmisc")
  
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
    # NumberAnyFall = aggregate(NumberAnyFall ~ KneeCategory, data=dfm, FUN=sum)[,"NumberAnyFall"],
    NumberFallsWithProsthesis = aggregate(NumberFallsWithProsthesis ~ KneeCategory, data=dfm, FUN=sum)[,"NumberFallsWithProsthesis"])
    # NumberFallsWithoutProsthesis = aggregate(NumberFallsWithoutProsthesis ~ KneeCategory, data=dfm, FUN=sum)[,"NumberFallsWithoutProsthesis"])

  row.names(dfskc) <- dfskc$KneeCategory

  # stop considering NA as a KneeCategory
  dfm$KneeCategory <- factor(dfm$KneeCategory, exclude =NA)


    # include in table dfskc the model-estimated fall rates and their 95% CI
  # unweighted  
  dfskc <- merge(dfskc, lIR$irci_unw, by="KneeCategory", all.x=T)
    dfskc[,"IR_NumberFallsWithProsthesis_CI"] <- paste0(round(dfskc$irr_l, digits=2), "-", round(dfskc$irr_u, digits=2))
    names(dfskc)[names(dfskc)=="irr"] <- "IR_NumberFallsWithProsthesis"
    dfskc <- dfskc[,setdiff(names(dfskc),c("irr_l","irr_u"))]
    # weighted
    dfskc <- merge(dfskc, lIR$irci_w, by="KneeCategory", all.x=T)
    names(dfskc)[names(dfskc)=="irr"] <- "IR_NumberFallsWithProsthesis_PS"
    dfskc[,"IR_NumberFallsWithProsthesis_PS_CI"] <- paste0(round(dfskc$irr_l, digits=2), "-", round(dfskc$irr_u, digits=2))
    
  dfskc$KneeCategory <- as.character(dfskc$KneeCategory)
  dfskc[nrow(dfskc)+1, "KneeCategory"] <- "All"
  
    dfskc[dfskc$KneeCategory=="All", "IR_NumberFallsWithProsthesis"] <- lIR$IR_all_unw$irr
    dfskc[dfskc$KneeCategory=="All", "IR_NumberFallsWithProsthesis_CI"] <- paste0(round(lIR$IR_all_unw$irr_l,digit=2), "-", round(lIR$IR_all_unw$irr_u,digit=2))
  
    
  # sort
  row.names(dfskc) <- dfskc$KneeCategory
  dfskc <- dfskc[c("LK","AMK","FK","MPK","NA","All"),]

  dfskc <- dfskc[,c("KneeCategory","NumberHospitalStays","NumberPatients",
                                      "NumberHospitalDays","NumberFallsWithProsthesis",
                                      "IR_NumberFallsWithProsthesis","IR_NumberFallsWithProsthesis_CI",
                                      "IR_NumberFallsWithProsthesis_PS","IR_NumberFallsWithProsthesis_PS_CI")]
  dfskc <- t(dfskc)
  
  return(dfskc)
}

