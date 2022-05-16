# constructPropensityScore_gbm


# construct GBM for propensity score
# using the built-in "surrogate split method"
constructPropensityScore_gbm <- function(dfr, varps){
  
  
  # import packages---------------
  require(twang)
  
  # pre-processing-------------------------
  
  dfm <- dfr[!is.na(dfr$KneeCategory),]
  
  dfprop <- dfm[,c("AnonymousID","AdmissionDate","KneeCategory",varps)]
  
  
  # # fill NAs with median or mode
  # getmode <- function(v) {
  #   uniqv <- unique(v)
  #   uniqv[which.max(tabulate(match(v, uniqv)))]
  # }
  # 
  # for (cvar in varps){
  #   if(is.factor(dfprop[,cvar])){
  #     dfprop[is.na(dfprop[,cvar]), cvar] <- getmode(dfprop[,cvar])
  #   } else{
  #     dfprop[is.na(dfprop[,cvar]), cvar] <- median(dfprop[,cvar], na.rm=T)
  #   }
  # }
  # 
  
 
  # propensity score-------------------------------------------
  
  # lformula <- list()
  # for (indk in 1:length(vkc)){
  #   ck <- vkc[indk]
  #   lformula[[indk]] <- as.formula(paste0(ck," ~ ",paste0(varps, collapse=" + ")))
  # }
  # names(lformula) <- vkc
  # 
  # gbm0 <- ps(formula=lformula[["LK"]], data=dfprop,
  #            n.trees=5000,
  #            interaction.depth=2,
  #            shrinkage=0.01,
  #            perm.test.iters=0,
  #            stop.method=c("es.mean","ks.max"),
  #            estimand = "ATE",
  #            verbose=FALSE)
  # 
  cformula <- as.formula(paste0("KneeCategory ~ ",paste0(varps, collapse=" + ")))
  psobj <- mnps(formula=cformula, 
                 data=dfprop,
                 n.trees=10000,
                 interaction.depth=2,
                 shrinkage=0.01,
                 perm.test.iters=0,
                 stop.method="es.mean", # c("es.max","ks.max")
                 estimand = "ATE",
                 verbose=T)
  
  dfprop$w <- get.weights(psobj, stop.method="es.mean")
  
  dfr <- merge(dfr, dfprop[,c("AnonymousID","AdmissionDate","w")],
               by=c("AnonymousID","AdmissionDate"), all.x=T)
  
  # IR with negative binomial models
  
  
  
  
  
  
  
  
  return(list(psdata=dfr,psmodel=psobj))
}


# https://cran.r-project.org/web/packages/twang/vignettes/twang.pdf




# construct GBM for propensity score
# imputing missing data before GBM fitting

# D. L. Coffman, J. Zhou, and X. Cai, "Comparison of methods for handling covariate missingness in propensity score estimation with a binary exposure," BMC Med. Res. Methodol. 2020 201, vol. 20, no. 1, pp. 1-14, Jun. 2020, doi: 10.1186/S12874-020-01053-4.

constructPropensityScore_imp_gbm <- function(dfr, varps){
  
  
  # import packages---------------
  require(mice)
  require(twang)
  
  # pre-processing-------------------------
  
  dfm <- dfr[!is.na(dfr$KneeCategory),]
  
  dfprop <- dfm[,c("AnonymousID","AdmissionDate","KneeCategory",varps)]
  
  
  # # fill NAs with median or mode
  # getmode <- function(v) {
  #   uniqv <- unique(v)
  #   uniqv[which.max(tabulate(match(v, uniqv)))]
  # }
  # 
  # for (cvar in varps){
  #   if(is.factor(dfprop[,cvar])){
  #     dfprop[is.na(dfprop[,cvar]), cvar] <- getmode(dfprop[,cvar])
  #   } else{
  #     dfprop[is.na(dfprop[,cvar]), cvar] <- median(dfprop[,cvar], na.rm=T)
  #   }
  # }
  # 
  
  # dfprop$FirstdeliveryRenewal_n <- NA
  # dfprop[dfprop$FirstdeliveryRenewal %in% "FirstDeliv","FirstdeliveryRenewal_n"] <- 0
  # dfprop[dfprop$FirstdeliveryRenewal %in% "Renewal","FirstdeliveryRenewal_n"] <- 1
  # dfprop <- dfprop[,setdiff(names(dfprop),"FirstdeliveryRenewal")]
  
  #tempDfprop <- mice(data=dfprop, m=1, method="norm.nob", seed=531)
  tempDfprop <- mice(data=dfprop, m=1, defaultMethod=c("norm.nob","logreg","polyreg","polr"), seed=135) #531
  dfprop_imp <- complete(tempDfprop)
  
  # names(dfprop_imp)[names(dfprop_imp)=="FirstdeliveryRenewal_n"] <- "FirstdeliveryRenewal"
  
  
  # # diagnostics
  # sapply(dfprop, class)
  # sapply(dfprop, function(x) sum(is.na(x)))
  # dfprop$TimeFromAmputation_logdays
  
  
  # propensity score-------------------------------------------
  
  # lformula <- list()
  # for (indk in 1:length(vkc)){
  #   ck <- vkc[indk]
  #   lformula[[indk]] <- as.formula(paste0(ck," ~ ",paste0(varps, collapse=" + ")))
  # }
  # names(lformula) <- vkc
  # 
  # gbm0 <- ps(formula=lformula[["LK"]], data=dfprop,
  #            n.trees=5000,
  #            interaction.depth=2,
  #            shrinkage=0.01,
  #            perm.test.iters=0,
  #            stop.method=c("es.mean","ks.max"),
  #            estimand = "ATE",
  #            verbose=FALSE)
  # 
  cformula <- as.formula(paste0("KneeCategory ~ ",paste0(varps, collapse=" + ")))
  psobj <- mnps(formula=cformula, 
                data=dfprop_imp,
                n.trees=10000,
                interaction.depth=2,
                shrinkage=0.01,
                perm.test.iters=0,
                stop.method="es.mean", # c("es.max","ks.max")
                estimand = "ATE",
                verbose=T)
  
  dfprop_imp$w <- get.weights(psobj, stop.method="es.mean")
  
  dfr <- merge(dfr, dfprop_imp[,c("AnonymousID","AdmissionDate","w")],
               by=c("AnonymousID","AdmissionDate"), all.x=T)
  
  
  return(list(psdata=dfr,psmodel=psobj))
}



