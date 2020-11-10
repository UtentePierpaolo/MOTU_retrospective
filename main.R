# main


# To fix:
# 1. update data on Figshare/Github
# 2. suppress verbose output
# 3. addresses and working directrory

rm(list=ls())
gc()

# load data and functions--------------------

# load data from local
# also present at 

setwd("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Code/MOTU_retrospective") 
source("preprocessing.R")
source("table_ExposureOutcome.R") 
source("association_xz.R")
source("constructPropensityScore_gbm.R")
source("f_univariate_associations.R")

# load data
# old version: https://figshare.com/s/c84ae0d82a3e053ab668
dfr <- read.csv("AnonymisedData_04112020.csv")


# pre-processing------------------------------

dfr <- preprocessing(dfr)

varx <- c("Age","Sex","ThirdPayer","FirstdeliveryRenewal","RehabGoal",
          "logTimeFromAmputation","AmputationSide","AmputationCause","StumpLength",
          "nComorbidities",
          "nDrugs","DrugAntipsychotics","DrugAntidepressants","DrugBenzodiazepines","DrugLoopDiuretics",
          "DrugBetaBlockingAgents","DrugOpioids","DrugAntiepileptics",
          "PainAverage",
          "LCIInitialScore",
          "AMPAdmissionScore","KlevelAdmission",
          "MorseAdmissionTotalScore", 
          "TWTInitialTime_m", "BarthelAdmissionTotalScore")

# # knee categories
# vkc <- c("LK","AMK","FK","MPK")
# 
# # color code for knee categories
# vcol= c("dark orange","dark green","light blue","brown")
# names(vcol) <- vkc


# descriptive statistics-----------------------

# Table 1. Descriptive statistics on the hospital stays.
# ...


# association xz------------------------------

# univariate associations between patient characteristics (x) and knee category (z)
lOR <- association_xz(dfr, varx)


# association xy----------------------------

# Table S2. Associations between characteristics of patients and their hospital stays, and any fall.
# univariate_associations.R

# "NumberAnyFall", "NumberFallsWithProsthesis"
dfRR <- f_univariate_associations_yRR(y="NumberFallsWithProsthesis", xs=varx, dfm=dfr)
# adjust p-values for multiple testing
dfRR[!is.na(dfRR$p), "p_BH"] <- na.omit(p.adjust(dfRR$p, method="BH"))
dfRR[which(dfRR$p_BH<0.05),]


# propensity score (PS)------------------------

# PS construction
varps <- c("FirstdeliveryRenewal","logTimeFromAmputation","nComorbidities","nDrugs",
           "DrugAntidepressants","DrugAntiepileptics","ThirdPayer")

# lPS <- constructPropensityScore(dfm=dfr, varps=varps)
lPS <- constructPropensityScore_gbm(dfr=dfr, varps=varps)

# PS diagnostics 
# ...

dfr <- lPS$psdata

# association zy-----------------------------

# Table 2. Rehabilitation stays, patients and time at risk, and falls according to knee category
lExposureOutcome <- table_ExposureOutcome(dfm=dfr)
Table2 <- lExposureOutcome[["tab"]]
# TO DO: select only columns of interest; calculate quantities on All

# Figure 2. Falls incidence rates (IR) per knee category (panels A-C) and incidence rate ratios (IRR) per knee category pairs (panels D-F)




