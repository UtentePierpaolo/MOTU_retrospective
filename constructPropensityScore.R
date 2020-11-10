# constructPropensityScore


constructPropensityScore <- function(dfm, varps){
  
  
  # import packages---------------
  require(nnet)
  
  
  # pre-processing-------------------------
  
  # Firstdelivery, cast to numeric to treat it as other dichotoums variables
  a <- rep(NA, nrow(dfm))
  a[dfm$FirstdeliveryRenewal %in% "FirstDeliv"] <- 1
  a[dfm$FirstdeliveryRenewal %in% "Renewal"] <- 0
  dfm$FirstdeliveryRenewal <- a
  
  
  # propensity score-------------------------------------------
  
  dfprop <- dfm[,c("AnonymousID","AdmissionDate","KneeCategory",varps)]
  
  # fill NAs with median or mode
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  for (cvar in varps){
    if(is.factor(dfprop[,cvar])){
      dfprop[is.na(dfprop[,cvar]), cvar] <- getmode(dfprop[,cvar])
    } else{
      dfprop[is.na(dfprop[,cvar]), cvar] <- median(dfprop[,cvar], na.rm=T)
    }
  }
  
  dfprop <- na.omit(dfprop)
  
  fit_nnet <- multinom(formula=paste0("KneeCategory ~ ",paste0(varps, collapse=" + ")), 
                       data=dfprop, model=T)
  
  
  dfm1 <- cbind(dfprop[,c("AnonymousID","AdmissionDate")], fit_nnet$fitted.values)
  names(dfm1)[names(dfm1)=="0"] <- "PropScore0"
  names(dfm1)[names(dfm1)=="1"] <- "PropScore1"
  names(dfm1)[names(dfm1)=="2"] <- "PropScore2"
  names(dfm1)[names(dfm1)=="3"] <- "PropScore3"
  
  dfm2 <- merge(dfm, dfm1, all.x=T, by=c("AnonymousID","AdmissionDate"))
  
  
  return(list(psdata=dfm2,model=fit_nnet))
}


# 
# # fit_nnet$fitted.values
# # which prosthesis, as a number
# wpn <- as.numeric(dfprop$KneeCategory)
# dfprop$propscore <- NA
# for(ind in 1:nrow(dfprop)){
#   dfprop$propscore[ind] <- fit_nnet$fitted.values[ind, wpn[ind]]
# }
# 
# wpn <- as.numeric(dfprop$KneeCategory)
# dfprop$propscore2 <- NA
# for(ind in 1:nrow(dfprop)){
#   dfprop$propscore2[ind] <- fit_nnet2$fitted.values[ind, wpn[ind]]
# }


