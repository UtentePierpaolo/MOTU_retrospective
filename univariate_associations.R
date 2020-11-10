# univariate_associations


# performs univariate associations between
# patient characteristics and falls

# also tests knee category

rm(list=ls())
gc()

# import packages, functions, and data---------------
require(lubridate)
require(lme4)
library("Hmisc")
require(car)
load("D:/MOTU/retrospective study data/Data/Fall.RData")
load("D:/MOTU/retrospective study data/Data/HospitalStay_v1.7.Rdata")
load("D:/MOTU/retrospective study data/Data/Patient_v4.RData")


# pre-processing-------------------------

# creates dfm with some variables
# among those: nfall, dfall
# n=number, d=dichotomized, wp=with prosthesis

# join
dfm0 <- merge(HospitalStay, Patient, by="PatientID")

# Age
dfm0$Age <- time_length(interval(dfm0$BirthDay, dfm0$AdmissionDate), unit="year")
# dfm0$AgeCategorized <- cut(dfm0$Age, c(18,30,40,50,60,70,80,+Inf), right=F)

# TimeFromAmputation
dfm0$TimeFromAmputation <- as.numeric(dfm0$AdmissionDate - dfm0$AmputationDate)
dfm0$logTimeFromAmputation <- log(as.numeric(dfm0$AdmissionDate - dfm0$AmputationDate))

# PainAverage
dfm0$PainAverage <- rowMeans(dfm0[,c("PainControlaterLimb","PainControlateralKnee","PainBack","PainStump","PainPhantomLimb")], na.rm=T)

# BarthelAdmissionTotalScoreStandard_Cat
dfm0$BarthelAdmissionTotalScoreStandard_Cat <- cut(dfm0$BarthelAdmissionTotalScoreStandard, c(0,80,100,+Inf),right=F)
dfm0$BarthelAdmissionTotalScoreStandard_Cat <- relevel(dfm0$BarthelAdmissionTotalScoreStandard_Cat, ref="[100,Inf)")

# StumpLength
dfm0[dfm0$StumpLength %in% c("DistalThird","KneeDisarticulation"),"StumpLength"] <- "DistalDisart"
dfm0$StumpLength <- factor(dfm0$StumpLength)
dfm0$StumpLength <- droplevels(dfm0$StumpLength)
dfm0$StumpLength <- relevel(dfm0$StumpLength, ref="ProximalThird")

# KlevelAdmission
levels(dfm0$KlevelAdmission)[levels(dfm0$KlevelAdmission) %in% c("0","1")] <- "0-1"
dfm0$KlevelAdmission <- relevel(dfm0$KlevelAdmission, ref="4")

# falls
# number of falls per hospital stay
Fall$nfall <- 1
aFall <- aggregate(nfall ~ PatientID+AdmissionDate, data=Fall, FUN="sum")
dfm <- merge(dfm0, aFall, all.x = T, by=c("PatientID","AdmissionDate"))
dfm[is.na(dfm$nfall), "nfall"] <- 0

# dichotomize
dfm$dfall <- (dfm$nfall>0)*1


vartotest <- c("Age","Sex","ThirdPayer","FirstdeliveryRenewal","RehabGoal",
               "logTimeFromAmputation","AmputationSide","AmputationCause","StumpLength",
               "nComorbidities",
               "nDrugs","DrugAntipsychotics","DrugAntidepressants","DrugBenzodiazepines","DrugLoopDiuretics",
               "DrugBetaBlockingAgents","DrugOpioids","DrugAntiepileptics",
               "PainAverage",
               "LCIInitialScore",
               "AMPAdmissionScore","KlevelAdmission",
               "MorseAdmissionHfall","MorseAdmissionPathologies","MorseAdmissionMobility","MorseAdmissionEndovenous",
               "MorseAdmissionTransfer","MorseAdmissionMental","MorseAdmissionTotalScore", 
               "TWTInitialTime_m","TWTInitialSteps_m",
               "BarthelAdmissionTotalScoreStandard_Cat")

# excluded: 
# "AgeCategorized", "AmputationCause_DB", "DrugNSAID", "AMPAdmissionProNopro", "KneeCategory"
# "Height","Weight",
# "PainControlaterLimb","PainControlateralKnee","PainBack","PainStump","PainPhantomLimb",
# # "BarthelAdmissionHygiene","BarthelAdmissionWash","BarthelAdmissionNutrition","BarthelAdmissionDress",
# "BarthelAdmissionIntestinalincont","BarthelAdmissionUrinaryincont","BarthelAdmissionToilet","BarthelAdmissionTransfer",
# "BarthelAdmissionWalk","BarthelAdmissionStairs","BarthelAdmissionWheelchair",
# "BarthelAdmissionTotalScoreStandard",

# old:
# "nDrugs","DrugPsycholeptics","DrugAntidepressants","DrugBenzoOpioidsAntiepil","DrugAntihypertDigitalis",

# initially, include also KneeCategory among the variables to test
# later, do it differently
# using a structured approach (testing only in interaction with significant patient features...)
# and exclude falls that happened when the patient was not wearing the prothesis


# estimate associations-----------------

# Poisson and logistic models
# 1. multilevel on all samples
# 2. non mltilevel, only on first delivery

# formula0 <- "nfall ~ offset(log(LengthOfStay)) + (1 | PatientID)"
# m0 <- glmer(cformula, data=dfm, family = poisson(link = "log"), nAGQ = 20)

dfRR <- data.frame(variable=NA, level=NA, RR=NA, RR_lb=NA, RR_ub=NA, p=NA, p_BH=NA)
inddf <- 1

for(ind in 1:length(vartotest)){
  
  # as it is implemented now, when glmer outputs a warning, results are not written in dfRR
  
  tryCatch(
    expr = {
      print(ind)
      print(vartotest[ind])
      
      cformula <- paste0("nfall ~ offset(log(LengthOfStay)) + ", vartotest[ind], " + (1 | PatientID)")
      cdfm <- na.omit(dfm[,c("nfall","LengthOfStay",vartotest[ind],"PatientID")])
      if (is.character(cdfm[,vartotest[ind]])){
        cdfm[,vartotest[ind]] <- factor(cdfm[,vartotest[ind]])
      }
      m1 <- glmer(cformula, data=cdfm, family = poisson(link = "log"), nAGQ = 20)
      t1 <- summary(m1)$coefficients
      
      # likelihood ratio test (I think so)
      # clrt <- drop1(m1,test="Chisq")
      # vp[ind] <- clrt$`Pr(Chi)`[2]
      # Analysis of deviance table
      caov <- Anova(m1)
      
      for (indl in 2:nrow(t1)){
        inddf <- inddf + 1
        if (is.factor(cdfm[,vartotest[ind]])){
          dfRR[inddf,"level"] <- rownames(t1)[indl]
        }
        dfRR[inddf,"RR"] <- exp(t1[indl,"Estimate"])
        dfRR[inddf,"RR_lb"] <- exp(t1[indl,"Estimate"]-qnorm(1-0.05/2)*t1[indl,"Std. Error"])
        dfRR[inddf,"RR_ub"] <- exp(t1[indl,"Estimate"]+qnorm(1-0.05/2)*t1[indl,"Std. Error"])
        dfRR[inddf,"CI"] <- paste0("(",round(dfRR[inddf,"RR_lb"],digit=2),"-",round(dfRR[inddf,"RR_ub"],digit=2),")")
        if (indl==2){
          dfRR[inddf,"variable"] <- vartotest[ind]
          # dfRR[inddf,"p"] <- clrt$`Pr(Chi)`[2]
          dfRR[inddf,"p"] <- caov$`Pr(>Chisq)`
        }
      }
      if (is.factor(cdfm[,vartotest[ind]])){
        inddf <- inddf + 1
        mcontr <- contrasts(cdfm[,vartotest[ind]])
        dfRR[inddf,"level"] <- names(rowSums(mcontr))[rowSums(mcontr)==0]
        dfRR[inddf,"RR"] <- 1
      }
      
    },
    error = function(e){ 
      message('Caught an error!')
      print(e)
    },
    warning = function(w){
      message('Caught a warning!')
      print(w)
    },
    finally = {
      # (Optional)
      # Do this at the end before quitting the tryCatch structure...
    }
  )
  
}

# adjust p-values for multiple testing
# vp_BH <- p.adjust(p=vp, method="BH") # this contains also p-values relative to models with warnings
dfRR[!is.na(dfRR$p), "p_BH"] <- na.omit(p.adjust(dfRR$p, method="BH"))

dfRR[which(dfRR$p_BH<0.05),]




# output results-------------------

# Variable, (level), RR, 95% CI, p_BH
# for more levels if the variable is categorical

