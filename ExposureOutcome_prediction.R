# ExposureOutcome_prediction

# Exposure: KneeCategory (z)
# Outcome: FallsWithProsthesis (y)

# Calculate incidence rates (IR) and their confidence intervals (CI)
# for both the unweighted and the PS-weighted cases


ExposureOutcome_prediction <- function(lmodels){
  
  source("fdrrs.R")
  
  # unweighted IR, whole sample
  newdata <- data.frame(LengthOfStay=1000, KneeCategory="All")
  IR_all_unw <- fdrr_mp(gmem=lmodels$lmodels_unw$model_all,
                                                    newdata = newdata, CIlevel=.95)
  
  # unweighted IR, per KneeCategory
  newdata <- data.frame(LengthOfStay=1000, KneeCategory=factor(c("LK","AMK","FK","MPK")))
  irci_unw <- fdrr_mp(gmem=lmodels$lmodels_unw$model0, 
                             newdata=newdata, CIlevel=0.95)
  
  # weighted IR, per KneeCategory
  newdata <- data.frame(LengthOfStay=1, KneeCategory=factor(c("LK","AMK","FK","MPK")))
  irci_w <- fdrr_svy_pred(ates0m=lmodels$lmodels_w$model0, newdata=newdata, ndays=1000, CIlevel=0.95)
  
  return(list(IR_all_unw=IR_all_unw, irci_unw=irci_unw, irci_w=irci_w))
}
