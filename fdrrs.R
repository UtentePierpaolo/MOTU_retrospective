
# fdrrs


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



fdrr_svy <- function(gmem, namesx=NULL){
  
  vc <- coef(gmem)
  vc <- vc[paste0("KneeCategory",vkc)]
  
  # vc <- vc["(Intercept)"] + c(0, vc[2:nrow(a$coefficients)])
  vsted <- summary(gmem)$coefficients[,"Std. Error"]
  vsted <- vsted[paste0("KneeCategory",vkc)]
  
  ndays <- 1000
  irr <- ndays*exp(vc)
  irr_u <- ndays*exp(vc+ vsted)
  irr_l <- ndays*exp(vc - vsted)
  if (is.null(namesx)){
    namesx <- vkc
  }
  data.estimates <- data.frame(irr=irr, irr_u=irr_u, irr_l=irr_l, KneeCategory=namesx)
  return(data.estimates)
}
