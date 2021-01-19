# propensityscore_diagnostics


# Diagnostics for propensity score

# see e.g.
# D. F. Mccaffrey et al., "A tutorial on propensity score estimation for multiple treatments using generalized boosted models," Stat. Med., vol. 32, no. 19, pp. 3388-3414, Aug. 2013, doi: 10.1002/sim.5753.

# load packages-----------------------
# require(lme4)
require(twang)

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



# boxplots for overlap assessment----------------------

# Propensity score for LK
boxplot(PropScore0 ~ KneeCategory, data=dfrp,
        ylab="Propensity score for LK", xaxt="n")
axis(side=1,at=1:4,labels=vkc)

# Propensity score for AMK
boxplot(PropScore1 ~ KneeCategory, data=dfrp,
        ylab="Propensity score for AMK", xaxt="n")
axis(side=1,at=1:4,labels=vkc)

# Propensity score for FK
boxplot(PropScore2 ~ KneeCategory, data=dfrp,
        ylab="Propensity score for FK", xaxt="n")
axis(side=1,at=1:4,labels=vkc)

# Propensity score for MPK
boxplot(PropScore3 ~ KneeCategory, data=dfrp,
        ylab="Propensity score for MPK", xaxt="n")
axis(side=1,at=1:4,labels=vkc)


# before and after weighting/adjusting plots----------------------

# pooled standardized bias PSB_k,t
# max_k(PSB_k,t)

vps <- paste0("PropScore",0:3)

# extend to all variables
# convarx <- varx[!sapply(dfrp[,varx], is.factor)]
convarx <- varps[!sapply(dfrp[,varps], is.factor)]
tbaa <- data.frame(ba=NA, aa=NA)

for (indv in 1:length(convarx)){
  
  cvar <- convarx[indv]
  
  dfrp0 <- na.omit(dfrp[,c(cvar,vps,"KneeCategory","AnonymousID")])
  
  mcvar <- mean(dfrp0[,cvar], na.rm=T)
  sdcvar <- sd(dfrp0[,cvar], na.rm=T)
  ta <- aggregate(dfrp0[,cvar], list(dfrp0$KneeCategory), mean)
  # before adjustemnt
  tbaa[indv, "ba"] <- max(abs(ta$x-mcvar)/sdcvar)
  
  
  # adjusting
  cformula_aa <- paste0(cvar," ~ ", "KneeCategory + ", paste("PropScore", 0:2, collapse=" + ", sep="")," + (1|AnonymousID)")
  cmod_aa <- lmer(cformula_aa, data=dfrp0)
  newdata <- data.frame(KneeCategory=c(0,1,2,3),
                        PropScore0=0, PropScore1=0, PropScore2=0, PropScore3=0)
  yhat_aa <- predict(object=cmod_aa, newdata, re.form=NA)
  # max(abs(yhat_aa - mean0)/sd0)
  tbaa[indv, "aa"] <- max(abs(yhat_aa-mcvar)/sdcvar)
  
}
row.names(tbaa) <- convarx


# cformula_ba <- paste0(cvar," ~ ", "KneeCategory + (1|AnonymousID)")
# cmod_ba <- lmer(cformula_ba, data=dfrp0)
# newdata <- data.frame(KneeCategory=c(0,1,2,3),
#                       PropScore0=0, PropScore1=0, PropScore2=0, PropScore3=0)
# yhat_ba <- predict(object=cmod_ba, newdata, re.form=NA)
# cformula_0 <- paste0(cvar," ~ 1 + (1|AnonymousID)")
# cmod_0 <- lmer(cformula_0, data=dfrp0)
# mean0 <- predict(cmod_0, newdata=data.frame(1), re.form=NA)
# vc <- VarCorr(cmod_0)
# sd0 <- sqrt(sum(c(unlist(lapply(vc,diag)), attr(vc, "sc")^2)))
# max(abs(yhat_ba - mean0)/sd0)

plot(x=c(0,1),y=tbaa[1,], ylim=range(tbaa), lty=2)
for(indv in 1:nrow(tbaa)){
  points(c(0,1), tbaa[indv,])
  lines(c(0,1), tbaa[indv,])
}

