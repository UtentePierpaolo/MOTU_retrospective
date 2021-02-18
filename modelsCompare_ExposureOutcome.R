# modelsCompare_ExposureOutcome

# output:
# table: model, package, BIC

# for reviewers only


rm(list=ls())
gc()

setwd("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Code/MOTU_retrospective/") 
source("preprocessing.R")
require("lme4")
require("GLMMadaptive")
require("glmmTMB")

# load data
dfr <- read.csv("AnonymisedData_04112020.csv")

# pre-processing
lpre <- preprocessing(dfr)
dfm <- lpre[["dfr"]]


# fit models-------------------

lmodel <- list()

# lme4
vformula1 <- paste("NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID)")
lmodel[["MEPoisson_lme4"]] <- glmer(vformula1, data=dfm, family = poisson(link = "log"), nAGQ = 20)

# GLMMadaptive
lmodel[["MEPoisson_GLMMadaptive"]] <-  mixed_model(fixed = NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory, 
                                                   random = ~ 1 | AnonymousID, 
                                                   data = dfm, family = poisson(),
                                                   control=list(iter_EM=60, iter_qN_outer=30, iter_qN=20, iter_qN_incr=20,
                                                                nAGQ=20, optimizer="nlminb"))
lmodel[["MENeginom_GLMMadaptive"]] <-  mixed_model(fixed = NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory, 
                                         random = ~ 1 | AnonymousID, 
                                         data = dfm, family = negative.binomial())
anova(lmodel[["MEPoisson_GLMMadaptive"]], lmodel[["MENeginom_GLMMadaptive"]])
# Negative Binomial not significantly better than Poisson

# glmmTMB
lmodel[["MEPoisson_glmmTMB"]] <-  glmmTMB(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID),
                                          data=dfm, family=poisson(), ziformula = ~1,
                                          control= glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
lmodel[["MENegBin2_glmmTMB"]] <-  glmmTMB(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + KneeCategory + (1|AnonymousID),
                                          data=dfm, family=nbinom2(), ziformula = ~1,
                                          control= glmmTMBControl(optimizer=optim, optArgs=list(method="BFGS")))
Anova(lmodel[["MEPoisson_glmmTMB"]])
anova(lmodel[["MEPoisson_glmmTMB"]],lmodel[["MENegBin2_glmmTMB"]])


# comprataive table--------------


tableCompare <- data.frame(model=NA, AIC=NA, BIC=NA, logLik=NA, deviance=NA, df.resid=NA)
for(indm in 1:length(lmodel)){
  tableCompare[indm, "model"] <- names(lmodel)[indm]
  tableCompare[indm, c("AIC","BIC","logLik","deviance","df.resid")] <- summary(lmodel[[indm]])$AICtab
}

tableCompare[1,"model"] <- "MEPoisson_lme4"
tableCompare[1, c("AIC","BIC","logLik","deviance","df.resid")] <- summary(lmodel[["MEPoisson_lme4"]])$AICtab

tableCompare[2,"model"] <- "MEPoisson_GLMMadaptive"
asc <- summary(lmodel[["MEPoisson_GLMMadaptive"]])
tableCompare[2,c("AIC","BIC")] <- asc[c("AIC","BIC")]
tableCompare[2,"logLik"] <- asc["logLik"][1]
tableCompare[2,"deviance"] <- -2*asc$logLik[1]

tableCompare[3,"model"] <- "MENeginom_GLMMadaptive"
asc <- summary(lmodel[["MENeginom_GLMMadaptive"]])
tableCompare[3,c("AIC","BIC")] <- asc[c("AIC","BIC")]
tableCompare[3,"logLik"] <- asc["logLik"][1]
tableCompare[3,"deviance"] <- -2*asc$logLik[1]

tableCompare[4,"model"] <- "MEPoisson_glmmTMB"
tableCompare[4, c("AIC","BIC","logLik","deviance","df.resid")] <- summary(lmodel[["MEPoisson_glmmTMB"]])$AICtab

tableCompare[5,"model"] <- "MENegBin2_glmmTMB"
tableCompare[5, c("AIC","BIC","logLik","deviance","df.resid")] <- summary(lmodel[["MENegBin2_glmmTMB"]])$AICtab


write.table(x=tableCompare,file="clipboard",
            row.names = F, sep="\t")
