# preprocessing


preprocessing <- function(dfr, dfvarx=NULL){
  
  vdates <- c("AdmissionDate","AmputationDate")
  for(cvdate in vdates){
    dfr[,cvdate] <- as.Date(dfr[,cvdate], format="%Y-%m-%d")
  }
  
  dfr$TimeFromAmputation_days <- as.numeric(dfr$AdmissionDate - dfr$AmputationDate)
  dfr$TimeFromAmputation_logdays <- log(dfr$TimeFromAmputation)
  dfr$TimeFromAmputation_months <- (dfr$TimeFromAmputation_days)/365.25*12
  
  if(!is.null(dfvarx)){
    # find the type of each x variable: categorical, dichotomous, continuous
    dfvarx <- data.frame(varx=varx, type=NA)
    for(indv in 1:length(varx)){
      cvar <- dfr[,varx[indv]]
      if((is.character(cvar) | is.factor(cvar)) & length(unique(cvar))>2){
        dfvarx[indv,"type"] <- "categorical"
      }else if(length(unique(cvar))==2){
        dfvarx[indv,"type"] <- "dichotomous"
      }else{
        dfvarx[indv,"type"] <- "continuous"
      }
    }
  }
  
  
  return(list(dfr=dfr, dfvarx=dfvarx))
}



