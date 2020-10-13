# TableFigures_FallsKneeCategory


# table with statistics on KneeCategory
# and associations with falls occurred with prosthesis

# number HS, patients, hospitals days, falls
# p-value for (KneeCategory vs nfallwp)

# ... + figures, p-values after controlling for possible confounders


rm(list=ls())
gc()


# load packages and data---------------------

require(lme4)
library("Hmisc")
require(car)
require(multcomp)
# require(pbkrtest), require(lmerTest) ?????
source("C:/Users/Pierpaolo/Dropbox/R/MOTU/responsescale_plotconfintglht.R")

load("D:/MOTU/retrospective study data/Data/Fall.RData")
load("D:/MOTU/retrospective study data/Data/HospitalStay_v1.4.Rdata")
# load("D:/MOTU/retrospective study data/Data/Patient_v4.RData")

nmin = 5 # minimum number of falls to calculate falls rate in a group

# pre-processing-------------------------

# creates dfm with some variables
# among those: nfall, dfall, nfallwp, dfallwp
# n=number, d=dichotomized, wp=with prosthesis, wop=without prosthesis

dfm0 <- HospitalStay

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

# # nfallwp contains the number of falls, with prosthesis + NA
# Fallwpm <- Fall[!(Fall$WearingProsthesis %in% 0),]
# aFallwpm <- aggregate(nfall ~ PatientID+AdmissionDate, data=Fallwpm, FUN="sum")
# names(aFallwpm)[names(aFallwpm)=="nfall"] <- "nfallwpm"
# dfm <- merge(dfm, aFallwpm, all.x = T, by=c("PatientID","AdmissionDate"))
# dfm[is.na(dfm$nfallwpm), "nfallwpm"] <- 0

# nfallwop contains the number of falls occurred without the prosthesis
Fallwop <- Fall[Fall$WearingProsthesis %in% 0,]
aFallwop <- aggregate(nfall ~ PatientID+AdmissionDate, data=Fallwop, FUN="sum")
names(aFallwop)[names(aFallwop)=="nfall"] <- "nfallwop"
dfm <- merge(dfm, aFallwop, all.x = T, by=c("PatientID","AdmissionDate"))
dfm[is.na(dfm$nfallwop), "nfallwop"] <- 0


# # dichotomize
dfm$dfall <- (dfm$nfall>0)*1
dfm$dfallwp <- (dfm$nfallwp>0)*1
# dfm$dfallwpm <- (dfm$nfallwpm>0)*1
dfm$dfallwop <- (dfm$nfallwop>0)*1

# treat NA on KneeCategory as a group
dfm$KneeCategory <- factor(dfm$KneeCategory, exclude = "")

dfm$one <- 1

# create table with statistics---------------

dfskc <- data.frame( # number of HS for each knee category
                    nhs = aggregate(one  ~ KneeCategory, data=dfm, FUN=sum)[,"one"], 
                    # number of different patients for each knee category
                    np = aggregate(one  ~ KneeCategory, data=unique(dfm[,c("PatientID","KneeCategory","one")]), FUN=sum)[,"one"],
                    # number of hospital days for each knee category
                    los= aggregate(LengthOfStay ~ KneeCategory, data=dfm, FUN=sum)[,"LengthOfStay"],
                    # falls: (number or dicotomized), (any, for sure with prosthesis, maybe with prosthesis)
                    nfall = aggregate(nfall ~ KneeCategory, data=dfm, FUN=sum)[,"nfall"],
                    dfall = aggregate(dfall  ~ KneeCategory, data=dfm, FUN=sum)[,"dfall"],
                    nfallwp = aggregate(nfallwp ~ KneeCategory, data=dfm, FUN=sum)[,"nfallwp"],
                    # dfallwp = aggregate(dfallwp  ~ KneeCategory, data=dfm, FUN=sum)[,"dfallwp"],
                    # nfallwpm = aggregate(nfallwpm ~ KneeCategory, data=dfm, FUN=sum)[,"nfallwpm"],
                    nfallwop = aggregate(nfallwop ~ KneeCategory, data=dfm, FUN=sum)[,"nfallwop"])
                    # dfallwpm = aggregate(dfallwpm  ~ KneeCategory, data=dfm, FUN=sum)[,"dfallwpm"])
row.names(dfskc) <- c(0:3, "NA") #levels(dfm$KneeCategory)
dfskc$KneeCategory <- c(0:3, "NA")

# stop considering NA as a KneeCategory
dfm$KneeCategory <- factor(dfm$KneeCategory, exclude =NA)

# fall rates----------------------

# crude fall rate (x 1000 hospital days)
dfskc$crudefallrate <- dfskc$nfall/dfskc$los*1000
dfskc$crudefallrate_wp <- dfskc$nfallwp/dfskc$los*1000
# dfskc$crudefallrate_wpm <- dfskc$nfallwpm/dfskc$los*1000
dfskc$crudefallrate_wop <- dfskc$nfallwop/dfskc$los*1000
# view
dfskc[,c("crudefallrate","crudefallrate_wp","crudefallrate_wop")]

voutcome <- c("nfall","nfallwp","nfallwop") # ,"nfallwpm"
vformula_all <- paste(voutcome, " ~ offset(log(LengthOfStay)) + (1|PatientID)")
vformula0 <- paste(voutcome, " ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)")
vformula1 <- paste(voutcome, " ~ offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)")
vaformula <- paste(voutcome, " ~ KneeCategory")
lmodel_all <- list()
lmodel0 <- list()
lmodel1 <- list()
lanova <- list()
for (ind in 1:length(voutcome)){
  # model on all HS
  lmodel_all[[ind]] <- glmer(vformula_all[ind], data=dfm, family=poisson(link="log"), nAGQ=20)
  
  # if on some knee category there are less than nmin(=5) falls, exclude that category
  adfm <- aggregate(as.formula(vaformula[ind]), data=dfm, FUN=sum)
  # knee category to exclude from the analyses
  kcte <- adfm[adfm[,voutcome[ind]] < nmin, "KneeCategory"]
  cdfm <- droplevels(dfm[!(dfm$KneeCategory %in% kcte),])
  
  lmodel0[[ind]] <- glmer(vformula0[ind], data=cdfm, family = poisson(link = "log"), nAGQ = 20)
  lmodel1[[ind]] <- glmer(vformula1[ind], data=cdfm, family = poisson(link = "log"), nAGQ = 20)
  lanova[[ind]] <- Anova(lmodel1[[ind]])
}
names(lmodel_all) <- voutcome
names(lmodel0) <- voutcome
names(lmodel1) <- voutcome
names(lanova) <- voutcome
# summary(lmodel0[["nfall"]])
# drop1(lmodel0[[1]], test="Chisq")
# Anova(lmodel0[[1]])

# models for fall rates on all HS


fdrr <- function(gmem, namesx=NULL){
  
  vc <- fixef(gmem)
  # vc <- vc["(Intercept)"] + c(0, vc[2:nrow(a$coefficients)])
  vsted <- summary(gmem)$coefficients[,"Std. Error"]
  ndays <- 1000
  irr <- ndays*exp(vc)
  irr_u <- ndays*exp(vc+ vsted) 
  irr_l <- ndays*exp(vc - vsted)
  if (is.null(namesx)){
    namesx <- as.numeric(levels(gmem@frame$KneeCategory))
  }
  data.estimates <- data.frame(irr=irr, irr_u=irr_u, irr_l=irr_l, KneeCategory=namesx)
  return(data.estimates)
}

lRR <- list()
lRR[["nfall"]] <- fdrr(gmem=lmodel0[["nfall"]])
lRR[["nfallwp"]] <- fdrr(gmem=lmodel0[["nfallwp"]])
lRR[["nfallwop"]] <- fdrr(gmem=lmodel0[["nfallwop"]])

# include in table dfskc the model-estimated fall rates and their 95% CI
for (coutcome in voutcome){
  dfskc <- merge(dfskc, lRR[[coutcome]], by="KneeCategory", all.x=T)
  names(dfskc)[names(dfskc)=="irr"] <- paste0("IR_",coutcome)
  dfskc[,paste0("IR_",coutcome,"_CI")] <- paste0(round(dfskc$irr_l, digits=2), "-", round(dfskc$irr_u, digits=2))
  names(dfskc)[names(dfskc)=="irr_u"] <- paste0("IR_",coutcome,"_u")
  names(dfskc)[names(dfskc)=="irr_l"] <- paste0("IR_",coutcome,"_l")
}

dfskc[nrow(dfskc)+1, "KneeCategory"] <- "All"
for (coutcome in voutcome){
  RR_all_cout <- fdrr(gmem=lmodel_all[[coutcome]], namesx="All")
  dfskc[dfskc$KneeCategory=="All", paste0("IR_", coutcome)] <- RR_all_cout$irr
  dfskc[dfskc$KneeCategory=="All", paste0("IR_",coutcome,"_CI")] <- paste0(round(RR_all_cout$irr_l,digit=2), "-", round(RR_all_cout$irr_u,digit=2))
}


# fplotrr <- function(data.estimates, main=""){
#   
#   x <- 1:nrow(data.estimates)
#   plot(x, data.estimates$irr, ylim=c(0,5), xaxt='n', 
#        xlab="Knee category", ylab="Number of falls per 1000 days", main=main,
#        cex.lab=1.1, cex.axis=1.1, cex.main=1.1, cex.sub=1.5)
#   axis(side = 1, at = x, labels=data.estimates$var, cex.lab=1.1, cex.axis=1.1)
#   errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
#          add=T,lwd=2, errbar.col="blue")
#   # points(x, dfskc$crudefallrate)
# }
# 
# fplotrr(fdrr(lmodel0[["nfall"]], levKneeCategory), "Any fall")
# fplotrr(fdrr(lmodel0[["nfallwpm"]], levKneeCategory), "Falls with prosthesis + NA")
# fplotrr(fdrr(lmodel0[["nfallwp"]], levKneeCategory), "Falls with prosthesis")


# in one same plot

png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/fall_KneeCategory_v2.png",
    width =7,height =6, units = 'in', res = 300)

data.estimates <- fdrr(gmem=lmodel0[["nfall"]])
x <- data.estimates$KneeCategory
plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3.2), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", 
     cex.lab=1.1, cex.axis=1.1, cex.main=1.1, cex.sub=1.5)
axis(side = 1, at = x, labels=data.estimates$var, cex.lab=1.1, cex.axis=1.1)
errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")

# data.estimates <- fdrr(lmodel0[["nfallwpm"]])
# x <- data.estimates$var
# errbar(x+0.1, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
#        add=T,lwd=2, errbar.col="dark grey", col="dark grey")

data.estimates <- fdrr(lmodel0[["nfallwp"]])
x <- data.estimates$KneeCategory
errbar(x+0.10, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark red", col="dark red")

data.estimates <- fdrr(gmem=lmodel0[["nfallwop"]])
x <- data.estimates$KneeCategory
errbar(x+0.2, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark green", col="dark green")

legend(x=0, y=5, legend=c("Any fall", "Falls with prosthesis","Falls without prosthesis"), # "Falls with prosth. + NA", 
       col=c("dark blue","dark red","dark green"), lty=1, pch=16, lwd=2) # "dark grey",

dev.off()



# use package multcomp

# # estimate fall probability and conf int with logistic mixed effect model
# cformula <- "dfall  ~ -1 + KneeCategory + (1|PatientID)"
# m1 <- glmer(cformula, data=dfm, family = binomial(), nAGQ = 20)
# K <- diag(length(fixef(m1)))
# ci <- confint(glht(m1, linfct = K))
# ci$confint <- binomial()$linkinv(ci$confint)
# plot(ci, xlim=c(0,1))


# # estimate fall rate and conf int with Poisson mixed effect model
# cformula <- "nfallwp  ~ -1 + offset(log(LengthOfStay)) + KneeCategory + (1|PatientID)"
# m1 <- glmer(cformula, data=dfm, family = poisson(link="log"), nAGQ = 20)
# K <- diag(length(fixef(m1)))
# ci <- confint(glht(m1, linfct = K))
# ci$confint <- poisson()$linkinv(ci$confint) *1000
# plot(ci, xlim=c(0,5))
# 
# # post-hoc comparisons
# ### set up all-pair comparisons for factor`tension'
# wht <- glht(m1, linfct = mcp(KneeCategory = "Tukey"))
# ### 95% simultaneous confidence intervals
# # plot(print(confint(wht)))
# plot(wht)
# summary(wht)
# summary(wht, test = univariate())
# summary(wht, test = adjusted("Shaffer"))
# summary(wht, test = adjusted("Westfall"))



# plots, one panel for each kind of fall-------------------------

# png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/fall_KneeCategory_v3.png",
#     width =15,height =6, units = 'in', res = 300)
# par(mfrow=c(2,3)) 

png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/fall_KneeCategory_v4.png",
    width =15,height =8, units = 'in', res = 300)

layout(matrix(1:6, 2, 3, byrow = F))

# plot for falls with prosthesis

data.estimates <- lRR[["nfallwp"]]
# overall p-value
op <- lanova[["nfallwp"]]$`Pr(>Chisq)`
# pairwise comparison
wht <- glht(lmodel1[["nfallwp"]], linfct = mcp(KneeCategory = "Tukey"))
summary(wht)

x <- data.estimates$KneeCategory
plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", main="Falls with prosthesis",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at = x, labels=data.estimates$var, cex.lab=1.5, cex.axis=1.5)
errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
# overall p -value
text(x=0.4,y=4.75, labels=paste("p=",as.character(round(op, digits=3))), cex=1.5)
# pairwise significant differences
# 2-0
lines(c(0,0,2,2),c(3,3.2,3.2,3), lwd=1.5)
text(x=1, y=3.35, label="*", cex=1.5)
# 3-2
lines(c(2,2,3,3),c(3.5,3.7,3.7,3.5), lwd=1.5)
text(x=2.5, y=3.85, label="**", cex=1.5)

fcg(confint(wht), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)


# falls without prosthesis

data.estimates <- lRR[["nfallwop"]]
# overall p-value
op <- lanova[["nfallwop"]]$`Pr(>Chisq)`
# pairwise comparison
wht <- glht(lmodel1[["nfallwop"]], linfct = mcp(KneeCategory = "Tukey"))
summary(wht)

x <- data.estimates$KneeCategory
plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", main="Falls without prosthesis",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
# axis(side = 1, at = x, labels=data.estimates$var, cex.lab=1.5, cex.axis=1.5)
axis(side = 1, at = 0:3, labels=0:3, cex.lab=1.5, cex.axis=1.5)
errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
# overall p -value
text(x=0.4,y=4.75, labels=paste("p=",as.character(round(op, digits=3))), cex=1.5)
# pairwise significant differences

fcg(confint(wht), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

# any fall

data.estimates <- lRR[["nfall"]]
# overall p-value
op <- lanova[["nfall"]]$`Pr(>Chisq)`
# pairwise comparison
wht <- glht(lmodel1[["nfall"]], linfct = mcp(KneeCategory = "Tukey"))
summary(wht)

x <- data.estimates$KneeCategory
plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3), xaxt='n', 
     xlab="Knee category", ylab="Number of falls per 1000 days", main="Any fall",
     cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(side = 1, at = x, labels=data.estimates$var, cex.lab=1.5, cex.axis=1.5)
errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
       add=T,lwd=2, errbar.col="dark blue", col="dark blue")
# overall p -value
text(x=0.4,y=4.75, labels=paste("p=",as.character(round(op, digits=3))), cex=1.5)
# pairwise significant differences
lines(c(2,2,3,3),c(4.5,4.7,4.7,4.5), lwd=1.5)
text(x=2.5, y=4.8, label="*", cex=1.5)

fcg(confint(wht), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)

dev.off()

