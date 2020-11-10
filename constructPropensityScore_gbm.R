# constructPropensityScore_gbm


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
                 n.trees=5000,
                 interaction.depth=2,
                 shrinkage=0.01,
                 perm.test.iters=0,
                 stop.method=c("es.max","ks.max"),
                 estimand = "ATE",
                 verbose=FALSE)
  
  dfprop$w <- get.weights(psobj, stop.method="es.max")
  
  dfr <- merge(dfr, dfprop[,c("AnonymousID","AdmissionDate","w")],
               by=c("AnonymousID","AdmissionDate"), all.x=T)
  
  
  
  return(list(psdata=dfr,psmodel=psobj))
}


# https://cran.r-project.org/web/packages/twang/vignettes/twang.pdf



# # diagnostics
# plot(psobj0)
# 
# summary(gbm0$gbm.obj,
#         n.trees=gbm0$desc$es.mean.ATE$n.trees,
#         plot=T)
# 
# bal.table(psobj0)
# 
# plot(psobj0, plots=2)
# plot(psobj0, plots=3)
