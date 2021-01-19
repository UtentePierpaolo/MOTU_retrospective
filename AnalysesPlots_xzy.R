# AnalysesPlots_xzy

# analyses and plots 

# x: "FirstdeliveryRenewal","logTimeFromAmputation","nComorbidities","nDrugs","DrugAntidepressants","DrugAntiepileptics","ThirdPayer"
# z: KneeCategory
# y: nfallwp

# ThirdPayer just because it is suspect

rm(list=ls())
gc()

# load packages and data--------------

require(lme4)
library("Hmisc")
library("plotrix")
require(car)
require(multcomp)
# require(pbkrtest), require(lmerTest) ?????

load("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/MOTU_Code_12.11/Fall.RData")
load("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/MOTU_Code_12.11/HospitalStay_v1.5.Rdata")
load("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/MOTU_Code_12.11/Patient_v4.RData")
# load("D:/MOTU/retrospective study data/Data/PropensityScore_v2.RData")


# pre-processing-------------------------

# creates dfm with some variables
# among those: nfall, dfall, nfallwp, dfallwp
# n=number, d=dichotomized, wp=with prosthesis

# join
dfm0 <- merge(HospitalStay, Patient, by="PatientID")
dfm0 <- merge(dfm0, dfprop[,c("PatientID","AdmissionDate","propscore","propscore2")], by=c("PatientID","AdmissionDate"), all.x=T)

# TimeFromAmputation
dfm0$TimeFromAmputation <- as.numeric(dfm0$AdmissionDate - dfm0$AmputationDate)
dfm0$logTimeFromAmputation <- log(as.numeric(dfm0$AdmissionDate - dfm0$AmputationDate))

# FirstDelivery
dfm0$FirstDelivery <- NA
dfm0[dfm0$FirstdeliveryRenewal %in% "FirstDeliv","FirstDelivery"] <- T
dfm0[dfm0$FirstdeliveryRenewal %in% "Renewal","FirstDelivery"] <- F

# falls
# number of falls per hospital stay
Fall$nfall <- 1
aFall <- aggregate(nfall ~ PatientID+AdmissionDate, data=Fall, FUN="sum")
dfm <- merge(dfm0, aFall, all.x = T, by=c("PatientID","AdmissionDate"))
dfm[is.na(dfm$nfall), "nfall"] <- 0

# nfallwp contains the number of falls that for sure occurred while wearing the prosthesis
Fallwp <- Fall[Fall$WearingProsthesis %in% 1,]
aFallwp <- aggregate(nfall ~ PatientID+AdmissionDate, data=Fallwp, FUN="sum")
names(aFallwp)[names(aFallwp)=="nfall"] <- "nfallwp"
dfm <- merge(dfm, aFallwp, all.x = T, by=c("PatientID","AdmissionDate"))
dfm[is.na(dfm$nfallwp), "nfallwp"] <- 0

# # dichotomize
# dfm$dfall <- (dfm$nfall>0)*1
# dfm$dfallwp <- (dfm$nfallwp>0)*1

# association analyses------------------------

vx <- c("FirstDelivery","logTimeFromAmputation","nComorbidities","nDrugs","DrugAntidepressants","DrugAntiepileptics","ThirdPayer",
        "propscore","propscore2")

# # dichotomic covariates
# vdx <- c("FirstDelivery","DrugAntidepressants","DrugAntiepileptics")

# 1st approach: split the dataset according to a covariate, run different models, and test significance of KneeCategory in each model
# 2nd approach: run one model for each covariate, defining groups according to x*z

nmin <- 5

excludeRareKneeCategory <- function(ds){
  # if on some knee category there are less than nmin(=5) falls, exclude that category
  # nmin <- 4
  adfm <- aggregate(nfallwp ~ KneeCategory, data=ds, FUN=sum)
  # knee category to exclude from the analyses
  kcte <- adfm[adfm[,"nfallwp"] < nmin, "KneeCategory"]
  cdfm <- droplevels(ds[!(ds$KneeCategory %in% kcte),])
  return(cdfm)
}


# approach 1 ----------------------------

cformula0 <- "nfallwp ~ offset(log(LengthOfStay)) + KneeCategory + (1 | PatientID)"
cformula1 <- "nfallwp ~ offset(log(LengthOfStay)) + KneeCategory"

# split the dataset (dsx0, dsx1) according to the dichotomic covariate
# remove knee cathegories where nfallwp < nmin
dsx0 <- dfm[which(!dfm$FirstDelivery), c("nfallwp","LengthOfStay","KneeCategory","PatientID")]
table(dsx0[,c("KneeCategory","nfallwp")], useNA="a")
dsx0 <- excludeRareKneeCategory(dsx0)
dsx1 <- dfm[which(dfm$FirstDelivery), c("nfallwp","LengthOfStay","KneeCategory")]
table(dsx1[,c("KneeCategory","nfallwp")], useNA="a")
dsx1 <- excludeRareKneeCategory(dsx1)

# perform y~z in both dsx0, dsx1 
mx0 <- glmer(cformula0, data=dsx0, family = poisson(link = "log"), nAGQ = 20)
mx1 <- glm(cformula1, data=dsx1, family = poisson(link = "log"))
summary(mx0)
Anova(mx0)
summary(mx1)
Anova(mx1)

# for (ind in 1:length(vdx)){
#   cformula <- paste0("nfallwp ~ offset(log(LengthOfStay)) + ", vdx[ind], " + (1 | PatientID)")
#   # split the dataset (dsx0, dsx1) according to the dichotomic covariate
#   dsx0 <- dfm[which(dfm$FirstDelivery), ]
#   ............
# }


# approach 2--------------------

# # define x*z groups using dummy variables
# cdfm <- na.omit(dfm[,c("nfallwp","KneeCategory","FirstDelivery","PatientID","LengthOfStay")]) 
# cdfm$dfallwp <- (cdfm$nfallwp>0)*1
# cdfm$one <- 1
# tte <- aggregate(cbind(one, dfallwp, nfallwp) ~ -1 + KneeCategory:FirstDelivery, data=cdfm, FUN=sum)
# tte$toexclude <- tte$nfallwp<nmin
# 
# cdfm <- merge(cdfm, tte[,c("KneeCategory","FirstDelivery","toexclude")], 
#               by=c("KneeCategory","FirstDelivery"), all.x=T)
# cdfm <- cdfm[!cdfm$toexclude, ]
# # modm <- model.matrix(nfallwp ~ -1 + KneeCategory:FirstDelivery, data=cdfm)
# # cte <- colSums(modm)
# # modm <- modm[,-which(cte==0)]
# m01 <- glm(nfallwp ~ offset(log(LengthOfStay)) -1 + KneeCategory:FirstDelivery, data=cdfm, family = poisson(link = "log"))
# summary(m01)
# # Anova(m01)

# # cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory*FirstDelivery + (1|PatientID) # did not converge
# cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory+FirstDelivery + (1|PatientID)
# mFirstDelivery01 <- glmer(cformula, data=dfm, family = poisson(link = "log"), nAGQ = 20)
# # cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory*FirstDelivery
# # mFirstDelivery01 <- glm(cformula, data=dfm, family = poisson(link = "log"))
# Anova(mFirstDelivery01)
# 
# cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory+logTimeFromAmputation + (1|PatientID)
# # cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory*logTimeFromAmputation + (1|PatientID) # did not converge
# mlogTimeFromAmputation01 <- glmer(cformula, data=dfm, family = poisson(link = "log"), nAGQ = 20)
# Anova(mlogTimeFromAmputation01)
# 
# # cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory*nComorbidities + (1|PatientID) # did not converge
# cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory+nComorbidities + (1|PatientID) 
# mnComorbidities01 <- glmer(cformula, data=dfm, family = poisson(link = "log"), nAGQ = 20)
# Anova(mnComorbidities01)
# 
# # cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory*nDrugs + (1|PatientID)
# cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory+nDrugs + (1|PatientID)
# mnDrugs01 <- glmer(cformula, data=dfm, family = poisson(link = "log"), nAGQ = 20)
# Anova(mnDrugs01)
# 
# # cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory*DrugAntidepressants + (1|PatientID)
# cformula <- nfallwp ~ offset(log(LengthOfStay)) + KneeCategory+DrugAntidepressants + (1|PatientID)
# mDrugAntidepressants01 <- glmer(cformula, data=dfm, family = poisson(link = "log"), nAGQ = 20)
# Anova(mDrugAntidepressants01)
# 
# # ..."DrugAntiepileptics", "ThirdPayer"


vformula <- paste0("nfallwp ~ offset(log(LengthOfStay)) + KneeCategory+", vx," + (1|PatientID)")
lmod01 <- list()
sdf <- data.frame(model=NA,var=NA,Chisq=NA,Df=NA)
sdf$`Pr(>Chisq)` <- NA
for (ind in 1:length(vx)){
  lmod01[[ind]] <- glmer(vformula[ind], data=dfm, family = poisson(link = "log"), nAGQ = 20)
  canova <- Anova(lmod01[[ind]])
  canova$model <- paste0("KneeCategory+",vx[ind])
  canova$var <- rownames(canova)
  sdf <- rbind(sdf,canova)
}




# plots----------

fdrr <- function(gmem, namesx=NULL){
  if(any(class(gmem)=="glmerMod")){
    vc <- fixef(gmem)
  } else if (any(class(gmem)=="glm")){
    vc <- coef(gmem)
  }
  # vc <- vc["(Intercept)"] + c(0, vc[2:nrow(a$coefficients)])
  vsted <- summary(gmem)$coefficients[,"Std. Error"]
  ndays <- 1000
  irr <- ndays*exp(vc)
  irr_u <- ndays*exp(vc+ vsted) 
  irr_l <- ndays*exp(vc - vsted)
  if (is.null(namesx) & any(class(gmem)=="glmerMod")){
    namesx <- as.numeric(levels(gmem@frame$KneeCategory))
  } else if(is.null(namesx) & any(class(gmem)=="glm")){
    namesx <- as.numeric(levels(gmem$model$KneeCategory))
  }
  data.estimates <- data.frame(irr=irr, irr_u=irr_u, irr_l=irr_l, var=namesx)
  return(data.estimates)
}


# FirstdeliveryRenewal
table(dfm$FirstdeliveryRenewal, useNA="a")
dfmx1 <- dfm[dfm$FirstdeliveryRenewal %in% "FirstDeliv" & dfm$KneeCategory!=3,c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx1$KneeCategory <- droplevels(dfmx1$KneeCategory)
dfmx2 <- dfm[dfm$FirstdeliveryRenewal %in% "Renewal",c("LengthOfStay","nfallwp","KneeCategory","PatientID")]

# dfmx1 <- dfm[dfm$FirstdeliveryRenewal %in% "FirstDeliv",c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
# dfmx1 <- excludeRareKneeCategory(dfmx1)
# dfmx2 <- dfm[dfm$FirstdeliveryRenewal %in% "Renewal",c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
# dfmx2 <- excludeRareKneeCategory(dfmx2)

cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20)

png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_ALL.png",
    width =21,height =12, units = 'in', res = 300)

# layout(matrix(1:6, 2, 3, by.row = T))

op <- par(mfrow = c(2,3), cex = 1)

data.estimates <- fdrr(m1)
x <- data.estimates$var
plot(x, data.estimates$irr, ylim=c(0,20), xlim=c(0, 3+0.2), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at = 0:3, labels=0:3, cex.lab=1.1, cex.axis=1.1)
errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x <- data.estimates$var
errbar(x+0.15, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")

legend(x=0,y=20, legend=c("First delivery", "Renewal"),
       col=c("dark green","dark orange"), lty=1, pch=16, lwd=2)

# dev.off()


# DrugAntidepressants
table(dfm$DrugAntidepressants, useNA="a")
dfmx1 <- dfm[dfm$DrugAntidepressants, c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx1 <- dfmx1[!(dfmx1$KneeCategory %in% 0),]
dfmx2 <- dfm[!dfm$DrugAntidepressants, c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20)

# png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_Antidepre_v3.png",
#     width =7,height =6, units = 'in', res = 300)
data.estimates <- fdrr(m1)
x1 <- data.estimates$var
# from <- 15
# to <- 20
# gap.plot(x1,data.estimates$irr, gap = c(from,to), type = "b", xlab="Knee category", ylab="Number of falls per 1000 days")
plot(x1, data.estimates$irr, ylim=c(0,20), xlim=c(0, 3+0.2), xaxt='n',
     xlab="Knee category", ylab="Number of falls per 1000 days",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at = 0:3, labels=0:3, cex.lab=1.1, cex.axis=1.1)
errbar(x1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x2 <- data.estimates$var
errbar(x2+0.15, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")
# 
# axis.break(2, from, breakcol="snow", style="gap")
# axis.break(2, from*(1+0.02), breakcol="black", style="slash")
# axis.break(4, from*(1+0.02), breakcol="black", style="slash")
# axis(2, at=from)

legend(x=1.5,y=20, legend=c("Use of antidepressants", "No use of antidepressants"),
       col=c("dark green","dark orange"), lty=1, pch=16, lwd=2)

# dev.off()


# DrugAntiepileptics
table(dfm$DrugAntiepileptics, useNA="a")
dfmx1 <- dfm[dfm$DrugAntiepileptics, c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx1 <- droplevels(dfmx1[!(dfmx1$KneeCategory %in% c(0,1)),])
dfmx2 <- dfm[!dfm$DrugAntiepileptics, c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
# fixed effect model
# m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
m1 <- glm(nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory, data=dfmx1, family = poisson(link = "log"))
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20)

# png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_Antiepileptics.png",
#     width =7,height =6, units = 'in', res = 300)
data.estimates <- fdrr(gmem=m1)
x1 <- data.estimates$var
plot(x1, data.estimates$irr, ylim=c(0,20), xlim=c(0, 3+0.2), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at =0:3, labels=0:3, cex.lab=1.1, cex.axis=1.1)
errbar(x1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x2 <- data.estimates$var
errbar(x2+0.15, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")
legend(x=0,y=20, legend=c("Use of antiepileptics", "No use of antiepileptics"),
       col=c("dark green","dark orange"), lty=1, pch=16, lwd=2)
# dev.off()


# TimeFromAmputation
# dichotomize logTimeFromAmputation
dfm$dTimeFromAmputation <- cut2(dfm$TimeFromAmputation, g=3) # cuts=mean(dfm$logTimeFromAmputation)
table(dfm$dTimeFromAmputation)
cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
cformulas <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory"
dfmx1 <- dfm[dfm$dTimeFromAmputation %in% levels(dfm$dTimeFromAmputation)[1], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx2 <- dfm[dfm$dTimeFromAmputation %in% levels(dfm$dTimeFromAmputation)[2], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx3 <- dfm[dfm$dTimeFromAmputation %in% levels(dfm$dTimeFromAmputation)[3], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx3 <- droplevels(dfmx3[!(dfmx3$KneeCategory %in% 1),])
m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20)
m3 <- glmer(cformula, data=dfmx3, family = poisson(link = "log"), nAGQ = 20)
# m2 <- glm(cformulas, data=dfmx2, family = poisson(link = "log"))
# m3 <- glm(cformulas, data=dfmx3, family = poisson(link = "log"))

# png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_TimeFromAmputation.png",
#     width =7,height =6, units = 'in', res = 300)
data.estimates <- fdrr(gmem=m1)
x1 <- data.estimates$var
plot(x1, data.estimates$irr, ylim=c(0,20), xlim=c(0, 3+0.2), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at =0:3, labels=0:3, cex.lab=1.5, cex.axis=1.5)
errbar(x1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x2 <- data.estimates$var
errbar(x2+0.1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")
data.estimates <- fdrr(m3)
x3 <- data.estimates$var
errbar(x3+0.2, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
legend(x=0,y=20, legend=c(paste("Time from amputation: 1st tertile:",levels(dfm$dTimeFromAmputation)[1],"days"), 
                          paste("Time from amputation: 2nd tertile:",levels(dfm$dTimeFromAmputation)[2],"days"),
                          paste("Time from amputation: 3rd tertile:",levels(dfm$dTimeFromAmputation)[3],"days")),
       col=c("dark green","dark orange","dark blue"), lty=1, pch=16, lwd=2)
# dev.off()


# nDrugs
dfm$dnDrugs <- cut2(dfm$nDrugs, g=3)
table(dfm$dnDrugs)
table(dfm[,c("nfallwp","dnDrugs","KneeCategory")])
cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
cformulas <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory"
dfmx1 <- dfm[dfm$dnDrugs %in% levels(dfm$dnDrugs)[1], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
table(dfmx1$nfallwp, dfmx1$KneeCategory, useNA="a")
dfmx2 <- dfm[dfm$dnDrugs %in% levels(dfm$dnDrugs)[2], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx2 <- droplevels(dfmx2[!(dfmx2$KneeCategory %in% 0),])
table(dfmx2$nfallwp, dfmx2$KneeCategory, useNA="a")
dfmx3 <- dfm[dfm$dnDrugs %in% levels(dfm$dnDrugs)[3], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
table(dfmx3$nfallwp, dfmx3$KneeCategory, useNA="a")

# m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
m1 <- glm(cformulas, data=dfmx1, family = poisson(link = "log"))
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20)
m3 <- glmer(cformula, data=dfmx3, family = poisson(link = "log"), nAGQ = 20)
# m2 <- glm(cformulas, data=dfmx2, family = poisson(link = "log"))
# m3 <- glm(cformulas, data=dfmx3, family = poisson(link = "log"))

# png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_nDrugs.png",
#     width =7,height =6, units = 'in', res = 300)
data.estimates <- fdrr(gmem=m1)
x1 <- data.estimates$var
plot(x1, data.estimates$irr, ylim=c(0,20), xlim=c(0, 3+0.2), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", 
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at =0:3, labels=0:3, cex.lab=1.5, cex.axis=1.5)
errbar(x1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x2 <- data.estimates$var
errbar(x2+0.1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")
data.estimates <- fdrr(m3)
x3 <- data.estimates$var
errbar(x3+0.2, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
legend(x=0,y=20, legend=c(paste("Number of drugs: 1st tertile:",levels(dfm$dnDrugs)[1]), 
                          paste("Number of drugs: 2nd tertile:",levels(dfm$dnDrugs)[2]),
                          paste("Number of drugs: 3rd tertile:",levels(dfm$dnDrugs)[3])),
       col=c("dark green","dark orange","dark blue"), lty=1, pch=16, lwd=2)
# dev.off()


# nComorbidities
dfm$dnComorbidities <- cut2(dfm$nComorbidities, g=3)
table(dfm$dnComorbidities)
table(dfm[,c("nfallwp","dnComorbidities","KneeCategory")])
cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
cformulas <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory"
dfmx1 <- dfm[dfm$dnComorbidities %in% levels(dfm$dnComorbidities)[1], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
table(dfmx1$nfallwp, dfmx1$KneeCategory, useNA="a")
dfmx2 <- dfm[dfm$dnComorbidities %in% levels(dfm$dnComorbidities)[2], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx2 <- droplevels(dfmx2[!(dfmx2$KneeCategory %in% 0),])
table(dfmx2$nfallwp, dfmx2$KneeCategory, useNA="a")
dfmx3 <- dfm[dfm$dnComorbidities %in% levels(dfm$dnComorbidities)[3], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
table(dfmx3$nfallwp, dfmx3$KneeCategory, useNA="a")

# m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
m1 <- glm(cformulas, data=dfmx1, family = poisson(link = "log"))
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20) #
# m3 <- glmer(cformula, data=dfmx3, family = poisson(link = "log"), nAGQ = 20) # 
# m2 <- glm(cformulas, data=dfmx2, family = poisson(link = "log"))
m3 <- glm(cformulas, data=dfmx3, family = poisson(link = "log"))

# png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_nComorbidities.png",
#     width =7,height =6, units = 'in', res = 300)
data.estimates <- fdrr(gmem=m1)
x1 <- data.estimates$var
plot(x1, data.estimates$irr, ylim=c(0,20), xlim=c(0, 3+0.2), xaxt='n',
     xlab="Knee category", ylab="Number of falls per 1000 days",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at =0:3, labels=0:3, cex.lab=1.5, cex.axis=1.5)
errbar(x1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x2 <- data.estimates$var
errbar(x2+0.1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")
data.estimates <- fdrr(m3)
x3 <- data.estimates$var
errbar(x3+0.2, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
legend(x=0,y=20, legend=c(paste("Number of comorbidities: 1st tertile:",levels(dfm$dnComorbidities)[1]),
                          paste("Number of comorbidities: 2nd tertile:",levels(dfm$dnComorbidities)[2]),
                          paste("Number of comorbidities: 3rd tertile:",levels(dfm$dnComorbidities)[3])),
       col=c("dark green","dark orange","dark blue"), lty=1, pch=16, lwd=2)
dev.off()


# propscore
dfm$dpropscore <- cut2(dfm$propscore, g=3)
table(dfm$dpropscore)
table(dfm[,c("nfallwp","dpropscore","KneeCategory")])
cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|Anonymous)"
cformulas <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory"
dfmx1 <- dfm[dfm$dpropscore %in% levels(dfm$dpropscore)[1], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
table(dfmx1$nfallwp, dfmx1$KneeCategory, useNA="a")
dfmx2 <- dfm[dfm$dpropscore %in% levels(dfm$dpropscore)[2], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
table(dfmx2$nfallwp, dfmx2$KneeCategory, useNA="a")
dfmx3 <- dfm[dfm$dpropscore %in% levels(dfm$dpropscore)[3], c("LengthOfStay","nfallwp","KneeCategory","PatientID")]
dfmx3 <- droplevels(dfmx3[!(dfmx2$KneeCategory %in% 1),])
table(dfmx3$nfallwp, dfmx3$KneeCategory, useNA="a")

m1 <- glmer(cformula, data=dfmx1, family = poisson(link = "log"), nAGQ = 20)
# m1 <- glm(cformulas, data=dfmx1, family = poisson(link = "log"))
m2 <- glmer(cformula, data=dfmx2, family = poisson(link = "log"), nAGQ = 20) #
# m3 <- glmer(cformula, data=dfmx3, family = poisson(link = "log"), nAGQ = 20) #
# m2 <- glm(cformulas, data=dfmx2, family = poisson(link = "log"))
m3 <- glm(cformulas, data=dfmx3, family = poisson(link = "log"))

png("C:/Users/Alice/OneDrive - Alma Mater Studiorum Università di Bologna/Personal_Health_Systems_Lab/MOTU/Figures/fall_KneeCategory_propscore.png",
    width =7,height =6, units = 'in', res = 300)
data.estimates <- fdrr(gmem=m1)
x1 <- data.estimates$var
plot(x1, data.estimates$irr, ylim=c(0,15), xlim=c(0, 3+0.2), xaxt='n',
     xlab="Knee category", ylab="Number of falls per 1000 days",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at =0:3, labels=0:3, cex.lab=1.5, cex.axis=1.5)
errbar(x1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")
data.estimates <- fdrr(m2)
x2 <- data.estimates$var
errbar(x2+0.1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark orange", col="dark orange")
data.estimates <- fdrr(m3)
x3 <- data.estimates$var
errbar(x3+0.2, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
legend(x=0,y=15, legend=c(paste("Propensity score: 1st tertile:",levels(dfm$dpropscore)[1]),
                          paste("Propensity score: 2nd tertile:",levels(dfm$dpropscore)[2]),
                          paste("Propensity score: 3rd tertile:",levels(dfm$dpropscore)[3])),
       col=c("dark green","dark orange","dark blue"), lty=1, pch=16, lwd=2)
dev.off()

