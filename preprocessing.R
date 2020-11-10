# preprocessing


preprocessing <- function(dfr){
  
  vdates <- c("AdmissionDate","AmputationDate")
  for(cvdate in vdates){
    dfr[,cvdate] <- as.Date(dfr[,cvdate], format="%Y-%m-%d")
  }
  
  dfr$TimeFromAmputation <- as.numeric(dfr$AdmissionDate - dfr$AmputationDate)
  dfr$logTimeFromAmputation <- log(dfr$TimeFromAmputation)
  
  
  return(dfr)
}



