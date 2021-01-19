# IncidenceRates_trials


rm(list=ls())
setwd("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Code/MOTU_retrospective/") 
source("preprocessing.R")
# load data
dfm <- read.csv("AnonymisedData_04112020.csv")
dfm <- preprocessing(dfm)

# crude
68/32213*1000
# 2.11

require(lme4)
cformula_all <- "NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + (1|AnonymousID)"
ame <- glmer(cformula_all, data=dfm, family=poisson(link="log"), nAGQ=20)
exp(fixef(ame))*1000
# 1.25

ameq <- glmer(cformula_all, data=dfm, family=quasipoisson(link="log"), nAGQ=20)
# error

afe <- glm("NumberFallsWithProsthesis ~ offset(log(LengthOfStay))", data=dfm, 
           family=poisson(link="log"))
exp(coef(afe))*1000
# 2.11

anber <- glmer.nb("NumberFallsWithProsthesis ~ offset(log(LengthOfStay)) + (1|AnonymousID)",
                  data=dfm, nAGQ=20)
# does not converge

require(MASS)
annb <- glm.nb("NumberFallsWithProsthesis ~ offset(log(LengthOfStay))",
               data=dfm)
exp(coef(annb))*1000
# 2.106828 

require(gee)
# sort data by AnonymousID
dfm <- dfm[order(dfm$AnonymousID),]

angee <- gee("NumberFallsWithProsthesis ~ offset(log(LengthOfStay))",
             data=dfm, family="poisson",id=AnonymousID)
exp(coef(angee))*1000
# 2.11

angee2 <- gee("NumberFallsWithProsthesis ~ offset(log(LengthOfStay))",
              data=dfm, family="poisson",id=AnonymousID,
              corstr = "exchangeable", scale.fix = TRUE, scale.value = 1)
exp(coef(angee2))*1000
# 2.11

angee4 <- gee("NumberFallsWithProsthesis ~ offset(log(LengthOfStay))",
              data=dfm, family=quasi(link="log",variance="constant"), #mu, mu^2
              id=AnonymousID,
              corstr = "exchangeable")
exp(coef(angee4))*1000
# 2.11

angee3 <- gee("NumberFallsWithProsthesis ~ offset(log(LengthOfStay))",
              data=dfm, family="poisson",id=AnonymousID,
              corstr="AR-M", Mv=1)
exp(coef(angee3))*1000

# require(pscl)
# azip <- zeroinfl(NumberFallsWithProsthesis~ offset(log(LengthOfStay)) | AnonymousID,
#                  data=dfm)

# https://stats.idre.ucla.edu/r/dae/zip/
# https://towardsdatascience.com/an-illustrated-guide-to-the-zero-inflated-poisson-model-b22833343057
# stan_glmer from the rstanarm package, mc-stan.org
# plm

# glmmADMB, glmmTMB

require(geeM)
dfm[is.na(dfm$LengthOfStay), "LengthOfStay"] <- mean(dfm$LengthOfStay, na.rm=T)
ageem1 <- geem(NumberFallsWithProsthesis ~ offset(log(LengthOfStay)), id=AnonymousID,
               data = dfm, family=poisson,corstr="exchangeable")
# ar1
exp(coef(ageem1))*1000

require(GLMMadaptive)
anba <- mixed_model(fixed = NumberFallsWithProsthesis ~ offset(log(LengthOfStay)), random = ~ 1 | AnonymousID, 
                    data = dfm,family = GLMMadaptive::negative.binomial())
exp(fixef(anba))*1000
# 1.95


require(glmmTMB)




