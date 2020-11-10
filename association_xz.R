# association_xz


# calculate univariate associations between single patient risk factors and prosthetic knee category

association_xz <- function(dfr, varx){
  
  # load functions
  source("f_univariate_associations.R")
  
  # check
  if(any(!(c(varx,"KneeCategory") %in% names(dfr)))){
    stop("some variables not defined in the dataframe")
  }
  
  
  dfr$LK <- dfr$KneeCategory == "LK"
  dfr$AMK <- dfr$KneeCategory == "AMK"
  dfr$FK <- dfr$KneeCategory == "FK"
  dfr$MPK <- dfr$KneeCategory == "MPK"
  
  dfOR0 <- f_univariate_associations_OR(y="LK", xs=varx, dfm=dfr) 
  dfOR1 <- f_univariate_associations_OR(y="AMK", xs=varx, dfm=dfr)
  dfOR2 <- f_univariate_associations_OR(y="FK", xs=varx, dfm=dfr)
  dfOR3 <- f_univariate_associations_OR(y="MPK", xs=varx, dfm=dfr)
  
  lOR <- list(dfOR0,dfOR1,dfOR2,dfOR3)
  names(lOR) <- 0:3
  
  return(lOR)
}
