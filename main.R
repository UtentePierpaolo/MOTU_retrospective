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

setwd("C:/Users/p-pie/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Code/MOTU_retrospective/") 
source("preprocessing.R")
source("table_xz.R")
source("constructPropensityScore_gbm.R")
source("f_univariate_associations.R")
# source("models_ExposureOutcome.R")
source("ExposureOutcome_modelFitting.R")
source("ExposureOutcome_inference.R")
source("ExposureOutcome_prediction.R")
source("table_ExposureOutcome.R") 
source("plot_ExposureOutcome.R")
source("IRxzy.R")
source("plot_xzy.R")


# load data
# old version: https://figshare.com/s/c84ae0d82a3e053ab668
dfr <- read.csv("AnonymisedData_21032021.csv",
                stringsAsFactors=T)


# pre-processing------------------------------

varx <- c("Age","Sex",
          "Weight", "Height",
          "FirstdeliveryRenewal","LengthOfStay","RehabGoal","ThirdPayer",
          "TimeFromAmputation_days","TimeFromAmputation_logdays","TimeFromAmputation_months",
          "AmputationSide","AmputationCause",
          "nComorbidities",
          "nDrugs","DrugAntipsychotics","DrugAntidepressants","DrugBenzodiazepines","DrugLoopDiuretics",
          "DrugBetaBlockingAgents","DrugOpioids","DrugAntiepileptics",
          "MorseAdmissionTotalScore", 
          "BarthelAdmissionTotalScore")

# "StumpLength", "PainAverage", "LCIInitialScore", "AMPAdmissionScore","KlevelAdmission", "TWTInitialTime_m"
# variables excluded because having >= 50% missing rate

lpre <- preprocessing(dfr, varx)
dfr <- lpre[["dfr"]]
dfvarx <- lpre[["dfvarx"]]
# Time from amputation: keep only TimeFromAmputation_months
cdfvarx <- dfvarx[-which(dfvarx$varx %in% c("TimeFromAmputation_days","TimeFromAmputation_logdays")),]

# knee categories
vkc <- c("LK","AMK","FK","MPK")

# descriptive statistics-----------------------

# Table 1. Descriptive statistics on the hospitalizations:
# all hospitalizations
# hospitalizations per knee category

tdfr <- dfr
tdfr$all <- factor("all")
tab_x <- table_xz(dfr=tdfr, dfvarx=cdfvarx, varz="all")
tab_x_print <- print_Tabxz(tab_x)

# Distribution of patients' characteristics per knee category
ltab_xProsth <- table_xz(dfr=dfr, dfvarx=cdfvarx, varz="KneeCategory")
ltab_xProsth_print <- print_Tabxz(ltab_xProsth)

# write.table(tab_x_print$all, "clipboard", sep="\t", row.names=FALSE)
# write.table(ltab_xProsth_print[["LK"]], "clipboard", sep="\t", row.names=FALSE)
# write.table(ltab_xProsth_print[["AMK"]], "clipboard", sep="\t", row.names=FALSE)
# write.table(ltab_xProsth_print[["FK"]], "clipboard", sep="\t", row.names=FALSE)
# write.table(ltab_xProsth_print[["MPK"]], "clipboard", sep="\t", row.names=FALSE)


# Satistics on any falls
# total number of falls
sum(dfr$NumberAnyFall)
# number of hospitalizations with falls
whf <- which(dfr$NumberAnyFall>0)
length(whf)
# number of patients with falls
length(unique(dfr[whf,"AnonymousID"]))



# association xy----------------------------

# Associations between patients' characteristics with any fall and falls with prosthesis

# Time from amputation: keep only TimeFromAmputation_logdays
cvarx <- setdiff(varx, c("TimeFromAmputation_days","TimeFromAmputation_months","LengthOfStay"))

# # "NumberAnyFall" (af)
# dfRR_af <- f_univariate_associations_yRR(y="NumberAnyFall", xs=cvarx, dfm=dfr)
# dfRR_af[which(dfRR_af$p<0.05),]

# "NumberFallsWithProsthesis" (wp)
dfRR_wp <- f_univariate_associations_yRR(y="NumberFallsWithProsthesis", xs=cvarx, dfm=dfr)
dfRR_wp[which(dfRR_wp$p<0.05),]



# propensity score (PS)------------------------

# PS construction
# varps <- c("FirstdeliveryRenewal","TimeFromAmputation_logdays","nComorbidities","nDrugs",
#            "DrugAntidepressants","DrugAntiepileptics","ThirdPayer")

varps <- dfRR_wp[which(dfRR_wp$p<0.05),"variable"]
# "FirstdeliveryRenewal"       "TimeFromAmputation_logdays" "AmputationCause"           
# "DrugAntidepressants"        "DrugAntiepileptics"


# lPS <- constructPropensityScore(dfm=dfr, varps=varps)
lPS <- constructPropensityScore_gbm(dfr=dfr, varps=varps)

dfr <- lPS$psdata

# PS diagnostics 
# 1. balance, 2. overlap, 


# balance
plot(lPS$psmodel)

# Distribution of patients' characteristics per knee category
# after propensity score weighting
ltab_xProsth_w <- table_xz(dfr=dfr, w="w", dfvarx=cdfvarx, varz="KneeCategory")
ltab_xProsth_w_print <- print_Tabxz(ltab_xProsth_w)

# table_PSbalance <- tab_PSbalance(t_pop=tab_x$all, t_unw=ltab_xProsth, t_w=ltab_xProsth_w)

# # overlap
# boxplot(PropScore2 ~ KneeCategory, data=dfrp,
#         ylab="Propensity score for FK", xaxt="n")
# axis(side=1,at=1:4,labels=vkc)



# association zy-----------------------------

# fit models for y vs z
# llmodelszy <- models_ExposureOutcome(dfm=dfr)

lmodels <- ExposureOutcome_modelFitting(dfm=dfr)

(linference <- ExposureOutcome_inference(lmodels))

lIR <- ExposureOutcome_prediction(lmodels)

# Table 2. Rehabilitation stays, patients and time at risk, and falls according to knee category
TableKneeFalls <- table_ExposureOutcome(dfm=dfr, lIR=lIR)

# Figure 1. Falls incidence rates (IR) per knee category and incidence rate ratios (IRR) per knee category pairs
plot_ExposureOutcome(linference=linference, lIR=lIR, 
                     saveaddress="C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/Publications/Manuscript_retrosp_1/Rev PTJ_2/Figures/")



# Figure 2. FallsWithProsthesis vs (KneeCategory x varps)
llIRxzy <- IRxzy(dfm=dfr)
plot_xzy(llIRxzy, saveaddress="C:/Users/p-pie/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/Publications/Manuscript_retrosp_1/Rev PTJ_2/Figures/")


# save.image(file = "results.RData")
