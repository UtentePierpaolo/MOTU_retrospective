# Computations4Reviewers

# Cause of amputation: % trauma per hospital stay and per patient
# Correlations

rm(list=ls())

setwd("C:/Users/p-pie/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Code/MOTU_retrospective") 
source("preprocessing.R")
source("models_ExposureOutcome_NegBin.R")

dfr <- read.csv("AnonymisedData_04112020.csv")
dfr <- preprocessing(dfr)
dfr <- dfr$dfr

# traumatic ampuation
# per hospital stay
table(dfr$AmputationCause, useNA="a")
sum(dfr$AmputationCause %in% "traumatic")/sum(!is.na(dfr$AmputationCause))
# 77%
# per patient
nptraumatic <- length(unique(dfr[dfr$AmputationCause %in% "traumatic","AnonymousID"]))
nptotal <- length(unique(dfr[!is.na(dfr$AmputationCause),"AnonymousID"]))
nptraumatic/nptotal
# 67%


# how many of them changed the types of the prosthetic knees during the renewal?
vids <- unique(dfr$AnonymousID)  
lpats <- list()
for (cid in vids){
  pdfr <- dfr[dfr$AnonymousID==cid, c("AnonymousID","AdmissionDate","KneeCategory")]
  pdfr <- pdfr[order(pdfr$AdmissionDate),]
  pdfr$prog <- 1:nrow(pdfr)
  pbefore <- pdfr
  pafter <- pdfr
  pafter$prog <- pafter$prog-1
  pba <- merge(pbefore, pafter, by=c("AnonymousID","prog"), all.x=T)
  pba$kcchanged <- pba$KneeCategory.x!=pba$KneeCategory.y
  lpats[[cid]] <- pba
}
# stack all couples of consecutives hospital stays one over the other
dchs <- lpats[[1]]
for (ind in 2:length(lpats)){
  dchs <- rbind(dchs, lpats[[ind]])
}
# exclude HS without a consequent one
dchs <- dchs[!is.na(dchs$AdmissionDate.y),]
table(dchs$kcchanged, useNA = "a")


# incidence rates with negative binomial models
lmodels <- models_ExposureOutcome_NegBin(dfm=dfr)

# correlations
cor(dfr$nComorbidities,dfr$MorseAdmissionTotalScore, use="p")
cor(dfr$nDrugs, dfr$DrugAntipsychotics, use="p")
cor(dfr$nDrugs, dfr$DrugAntidepressants, use="p")
cor(dfr$nDrugs, dfr$DrugAntiepileptics, use="p")

# time from amputation, firstDelivery/Renewal
w1 <- dfr$FirstdeliveryRenewal %in% "FirstDeliv"
w2 <- dfr$FirstdeliveryRenewal %in% "Renewal"
layout(matrix(data=c(1,2),nrow=1))
hist(dfr[w1,"TimeFromAmputation_months"],
     50, xlab="Time from amputation (months)",
     main="First prosthetic provision")
hist(dfr[w2,"TimeFromAmputation_months"],50, 
     xlab="Time from amputation (months)",
     main="Prosthesis renewal")

table(dfr$DrugAntidepressants, dfr$DrugAntiepileptics, useNA="a")
