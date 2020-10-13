# constructPropensityScore


rm(list=ls())
gc()

# import packages, functions, and data---------------
require(lubridate)
require(nnet)
load("D:/MOTU/retrospective study data/Data/HospitalStay_v1.5.Rdata")
load("D:/MOTU/retrospective study data/Data/Patient_v4.RData")


# pre-processing-------------------------

# join
dfm0 <- merge(HospitalStay, Patient, by="PatientID")

# Age
dfm0$Age <- time_length(interval(dfm0$BirthDay, dfm0$AdmissionDate), unit="year")
dfm0$AgeCategorized <- cut(dfm0$Age, c(18,30,40,50,60,70,80,+Inf), right=F)

# TimeFromAmputation
dfm0$TimeFromAmputation <- as.numeric(dfm0$AdmissionDate - dfm0$AmputationDate)
dfm0$logTimeFromAmputation <- log(as.numeric(dfm0$AdmissionDate - dfm0$AmputationDate))

# PainAverage
dfm0$PainAverage <- rowMeans(dfm0[,c("PainControlaterLimb","PainControlateralKnee","PainBack","PainStump","PainPhantomLimb")], na.rm=T)

# Firstdelivery, cast to numeric
a <- rep(NA, nrow(dfm0))
a[dfm0$FirstdeliveryRenewal %in% "FirstDeliv"] <- 1
a[dfm0$FirstdeliveryRenewal %in% "Renewal"] <- 0
dfm0$FirstdeliveryRenewal <- a


# propensity score-------------------------------------------


# relevel()...
varforprop <- c("FirstdeliveryRenewal","nComorbidities","DrugAntidepressants","DrugAntiepileptics","ThirdPayer","Age")
varforprop2 <- c("FirstdeliveryRenewal","logTimeFromAmputation","nComorbidities","nDrugs",
                 "DrugAntidepressants","DrugAntiepileptics","ThirdPayer")

dfprop <- dfm0[,unique(c("PatientID","AdmissionDate","KneeCategory",varforprop,varforprop2))]

# fill NAs with median or mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

for (cvar in varforprop){
  if(is.character(dfprop[,cvar])){
    dfprop[is.na(dfprop[,cvar]), cvar] <- getmode(dfprop[,cvar])
  } else{
    dfprop[is.na(dfprop[,cvar]), cvar] <- median(dfprop[,cvar], na.rm=T)
  }
}

dfprop <- na.omit(dfprop)

cformula <- paste0("KneeCategory ~ ",paste0(varforprop, collapse=" + "))
fit_nnet <- multinom(cformula, data=dfprop)
coef(fit_nnet)

cformula2 <- paste0("KneeCategory ~ ",paste0(varforprop2, collapse=" + "))
fit_nnet2 <- multinom(cformula2, data=dfprop)
coef(fit_nnet2)
sf2 <- summary(fit_nnet2)
write.table(sf2$coefficients, file="clipboard", sep="\t")
write.table(sf2$standard.errors, file="clipboard", sep="\t")



# fit_nnet$fitted.values
# which prosthesis, as a number
wpn <- as.numeric(dfprop$KneeCategory)
dfprop$propscore <- NA
for(ind in 1:nrow(dfprop)){
  dfprop$propscore[ind] <- fit_nnet$fitted.values[ind, wpn[ind]]
}

wpn <- as.numeric(dfprop$KneeCategory)
dfprop$propscore2 <- NA
for(ind in 1:nrow(dfprop)){
  dfprop$propscore2[ind] <- fit_nnet2$fitted.values[ind, wpn[ind]]
}


# save results---------------

# notes <- "created with constructPropensityScore.R on 20th April 2020"
# save(file="D:/MOTU/retrospective study data/Data/PropensityScore_v1.RData",
#      list=c("notes","dfprop"))

notes <- "created with constructPropensityScore.R on 23rd April 2020"
save(file="D:/MOTU/retrospective study data/Data/PropensityScore_v2.RData",
     list=c("notes","dfprop"))
