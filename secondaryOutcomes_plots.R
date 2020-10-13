# secondaryOutcomes_plots


rm(list=ls())
gc()

# load packages and data-----------------

require(lubridate)
require(lme4)
require(car)
# require(pbkrtest) # yet to be used
require(multcomp)

# load("D:/MOTU/retrospective study data/Data/Fall.RData")
load("D:/MOTU/retrospective study data/Data/HospitalStay_v1.6.Rdata")
load("D:/MOTU/retrospective study data/Data/Patient_v4.RData")

addTrans <- function(color,trans){
  # This function adds transparancy to a color.
  # Define transparancy with an integer between 0 and 255
  # 0 being fully transparant and 255 being fully visable
  # Works with either color and trans a vector of equal length,
  # or one of the two of length 1.
  
  if (length(color)!=length(trans)&!any(c(length(color),length(trans))==1)) stop("Vector lengths not correct")
  if (length(color)==1 & length(trans)>1) color <- rep(color,length(trans))
  if (length(trans)==1 & length(color)>1) trans <- rep(trans,length(color))
  
  num2hex <- function(x)
  {
    hex <- unlist(strsplit("0123456789ABCDEF",split=""))
    return(paste(hex[(x-x%%16)/16+1],hex[x%%16+1],sep=""))
  }
  rgb <- rbind(col2rgb(color),trans)
  res <- paste("#",apply(apply(rgb,2,num2hex),2,paste,collapse=""),sep="")
  return(res)
}



# settings for secondary outcomes------------------

# secondary outcomes, defined as improvements upon different scales
lso <- list(  LCI= c("LCIInitialScore","LCIDischargeScore"),
              AMP= c("AMPAdmissionScore","AMPDischargeScore"),
              # Klevel= c("KlevelAdmission","KlevelDischarge"),
              TWTTime= c("TWTInitialTime_m","TWTDischargeTime_m"),
              TWTSteps= c("TWTInitialSteps_m","TWTDischargeSteps_m"),
              TWTGaitspeed= c("TWTInitialGaitspeed","TWTDischargeGaitspeed"),
              Morse= c("MorseAdmissionTotalScore","MorseDischargeTotalScore"),
              Barthel= c("BarthelAdmissionTotalScoreStandard","BarthelDischargeTotalScoreStandard"),
              BarthelWalk= c("BarthelAdmissionWalk","BarthelDischargeWalk"),
              BarthelStairs= c("BarthelAdmissionStairs","BarthelDischargeStairs"))
mlso <- t(as.data.frame(lso))
colnames(mlso) <- c("Baseline","Discharge")

# which is negatively oriented
vno <- rep(1, length(lso))
names(vno) <- names(lso)
vno[c("TWTTime","TWTSteps","Morse")] <- -1

# settings for regressors-----------------

# join
dfm <- merge(HospitalStay, Patient, by="PatientID")

# Age
dfm$Age <- time_length(interval(dfm$BirthDay, dfm$AdmissionDate), unit="year")
# AgeCategorized
dfm$AgeCategorized <- cut(dfm$Age, c(18,30,40,50,60,70,80,+Inf), right=F)

# TimeFromAmputation
dfm$TimeFromAmputation <- as.numeric(dfm$AdmissionDate - dfm$AmputationDate)
dfm$logTimeFromAmputation <- log(as.numeric(dfm$AdmissionDate - dfm$AmputationDate))

# PainAverage
dfm$PainAverage <- rowMeans(dfm[,c("PainControlaterLimb","PainControlateralKnee","PainBack","PainStump","PainPhantomLimb")], na.rm=T)

# TWT gait speed
wlength <- 12
dfm$TWTInitialGaitspeed <- wlength/dfm$TWTInitialTime_m
dfm$TWTDischargeGaitspeed <- wlength/dfm$TWTDischargeTime_m


# plots-------

vkc <- addNA(dfm$KneeCategory)
vcol= c("dark orange","dark green","light blue","brown","black")
vpch= c(16, 17)


# just the legend for the scatterplots
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/Legend_scatter_2.png",
    width =7,height =6, units = 'in', res = 300)
plot(x=NULL,y=NULL, ylim=c(0,2), xlim=c(1,4),xaxt='n',yaxt='n',xlab="",ylab="")
legend(x=1.25,y=1.8, legend=c("LK","AMK","FCK","MPK","NA"), title="Knee category ", col=vcol, pch=15, box.col="white", cex=1.5)
legend(x=2.2,y=1.82, legend=c("First provision","Renewal"), col="dark grey", pch=vpch, box.col="white", cex=1.5)
rect(xleft=1.2, ybottom=0.7, xright=3.5, ytop=1.825)
dev.off()



# AMP---------------------------------------------
ylim <- c(0,47) 
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/AMP_scatter.png",
    width =7,height =6, units = 'in', res = 300)
plot(x=ylim,y=ylim, type="l",col="dark grey", 
     # xlim=range(dfm$AMPAdmissionScore, na.rm=T), ylim=range(dfm$AMPDischargeScore, na.rm=T),
     xlim=ylim, ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="AMP at T0", ylab="AMP at T2")
points(dfm$AMPAdmissionScore, dfm$AMPDischargeScore, col=addTrans(vcol[vkc],200), pch=vpch[as.factor(dfm$FirstdeliveryRenewal)])
# legend(x=30,y=19, legend=c(levels(dfm$KneeCategory),"NA"), col=vcol, title="Knee category", pch=15, box.col="white", cex=1.5)
# legend(x=38,y=20, legend=c("First provision","Renewal"), col="dark grey", pch=vpch, box.col="white", cex=1.5)
# rect(xleft=29, ybottom=0, xright=48, ytop=20)
dev.off()

# boxplot - first provision
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/AMP_box_FirsProv.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(AMPAdmissionScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, boxwex=0.2, main="First prosthetic provision", xlab="Knee category", ylab="AMP score",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=16, outcol=addTrans(vcol,200))
boxplot(AMPDischargeScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=16, outcol=addTrans(vcol,200))
text(x=1:4, y=2, labels="T0", cex=1.5)
text(x=(1:4)+0.3, y=2, labels="T2", cex=1.5)
dev.off()

# boxplot - renewal
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/AMP_box_Renewal.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(AMPAdmissionScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, boxwex=0.2, main="Prosthetic renewal", xlab="Knee category", ylab="AMP score",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=17, outcol=addTrans(vcol,200))
boxplot(AMPDischargeScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=17, outcol=addTrans(vcol,200))
text(x=1:4, y=2, labels="T0", cex=1.5)
text(x=(1:4)+0.3, y=2, labels="T2", cex=1.5)
dev.off()



# TWT time----------------------------------
plot(x=c(5,80),y=c(5,80), type="l",col="dark grey", 
     xlim=range(dfm[,mlso[inds,"Baseline"]], na.rm=T), ylim=range(dfm[,mlso[inds,"Discharge"]], na.rm=T),
     xlab="TWT on first day out of parallel bars (s)", ylab="TWT at discharge (s)")
points(dfm[,mlso[inds,"Baseline"]], dfm[,mlso[inds,"Discharge"]], col=vcol[vkc], pch=16)
legend(x=100,y=30, legend=c(levels(dfm$KneeCategory),"NA"), col=vcol, pch=16, title="Knee category")


# TWT gait speed-----------------

# plot(x=c(0.2,2),y=c(0.2,2), type="l",col="dark grey", 
#      xlim=range(dfm$TWTInitialGaitspeed, na.rm=T), ylim=range(dfm$TWTDischargeGaitspeed, na.rm=T),
#      xlab="Gait speed (m/s) out of parallel bars", ylab="Gait speed (m/s) at discharge")
# points(dfm$TWTInitialGaitspeed, dfm$TWTDischargeGaitspeed, col=addTrans(vcol[vkc],200), pch=16)
# legend(x=1.8,y=1, legend=c(levels(dfm$KneeCategory),"NA"), col=vcol, pch=16, title="Knee category")

ylim= c(0.15,2)

png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/GaitSpeed_scatter.png",
    width =7,height =6, units = 'in', res = 300)
plot(x=ylim,y=ylim, type="l",col="dark grey", 
     xlim=range(dfm$TWTInitialGaitspeed, na.rm=T), ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Gait speed (m/s) at T1", ylab="Gait speed (m/s) at T2")
points(dfm$TWTInitialGaitspeed, dfm$TWTDischargeGaitspeed, col=addTrans(vcol[vkc],200), pch=vpch[as.factor(dfm$FirstdeliveryRenewal)])
# legend(x=1.3,y=0.8, legend=paste0("Knee category ",c(0:3,"NA")), col=vcol, pch=15, box.col="white")
legend(x=1.25,y=0.8, legend=c(0:3,"NA"), title="Knee category ", col=vcol, pch=15, box.col="white")
legend(x=1.7,y=0.81, legend=c("First provision","Renewal"), col="dark grey", pch=vpch, box.col="white")
rect(xleft=1.2, ybottom=0.175, xright=2.25, ytop=0.825)
dev.off()

png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/GaitSpeed_scatter_2.png",
    width =7,height =6, units = 'in', res = 300)
plot(x=ylim,y=ylim, type="l",col="dark grey", 
     xlim=range(dfm$TWTInitialGaitspeed, na.rm=T), ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Gait speed (m/s) at T1", ylab="Gait speed (m/s) at T2")
points(dfm$TWTInitialGaitspeed, dfm$TWTDischargeGaitspeed, col=addTrans(vcol[vkc],200), pch=vpch[as.factor(dfm$FirstdeliveryRenewal)])
dev.off()

# boxplot - first provision
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/GaitSpeed_box_FirsProv.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(TWTInitialGaitspeed ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, boxwex=0.2, main="First prosthetic provision", xlab="Knee category", ylab="Gait speed (m/s)",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=16, outcol=addTrans(vcol,200))
boxplot(TWTDischargeGaitspeed ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=16, outcol=addTrans(vcol,200))
text(x=1:4, y=2, labels="T1", cex=1.5)
text(x=(1:4)+0.3, y=2, labels="T2", cex=1.5)
dev.off()
# boxplot - renewal
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/GaitSpeed_box_Renewal.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(TWTInitialGaitspeed ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, boxwex=0.2, main="Prosthetic renewal", xlab="Knee category", ylab="Gait speed (m/s)",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=17, outcol=addTrans(vcol,200))
boxplot(TWTDischargeGaitspeed ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=17, outcol=addTrans(vcol,200))
text(x=1:4, y=2, labels="T1", cex=1.5)
text(x=(1:4)+0.3, y=2, labels="T2", cex=1.5)
dev.off()

# boxplot - delta
plot(x=NULL,y=NULL, xlim=c(1,4),ylim=c(-0.4,0.8), xaxt="n",yaxt="n",xlab="",ylab="")
abline(h=0, col="grey")
boxplot(TWTDelta_GaitSpeed ~ KneeCategory, data=dfm, 
        col=vcol, boxwex=0.2, xlab="Knee category", ylab="Delta gait speed (m/s)",
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=17, outcol=addTrans(vcol,200), add=T)


# LCI---------------------

ylim= c(0,60)

png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/LCI_scatter.png",
    width =7,height =6, units = 'in', res = 300)
plot(x=ylim,y=ylim, type="l",col="dark grey", 
     xlim=ylim, ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="LCI at T1", ylab="LCI at T2")
points(dfm$LCIInitialScore, dfm$LCIDischargeScore, col=addTrans(vcol[vkc],200), pch=vpch[as.factor(dfm$FirstdeliveryRenewal)])
dev.off()

# boxplot - first provision
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/LCI_box_FirsProv.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(LCIInitialScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, boxwex=0.2, main="First prosthetic provision", xlab="Knee category", ylab="LCI",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=16, outcol=addTrans(vcol,200))
boxplot(LCIDischargeScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=16, outcol=addTrans(vcol,200))
text(x=1:4, y=2, labels="T1", cex=1.5)
text(x=(1:4)+0.3, y=2, labels="T2", cex=1.5)
dev.off()
# boxplot - renewal
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/LCI_box_Renewal.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(LCIInitialScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, boxwex=0.2, main="Prosthetic renewal", xlab="Knee category", ylab="LCI",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=17, outcol=addTrans(vcol,200))
boxplot(LCIDischargeScore ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=17, outcol=addTrans(vcol,200))
text(x=1:4, y=2, labels="T1", cex=1.5)
text(x=(1:4)+0.3, y=2, labels="T2", cex=1.5)
dev.off()


# Barthel--------------

ylim <- c(28,100)

png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/Barthel_scatter.png",
    width =7,height =6, units = 'in', res = 300)
plot(x=ylim,y=ylim, type="l",col="dark grey", 
     xlim=ylim, ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5,
     xlab="Barthel Index at T0", ylab="Barthel Index at T2")
points(dfm$BarthelAdmissionTotalScoreStandard, dfm$BarthelDischargeTotalScoreStandard, 
       col=addTrans(vcol[vkc],200), pch=vpch[as.factor(dfm$FirstdeliveryRenewal)])
dev.off()

# boxplot - first provision
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/Barthel_box_FirsProv.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(BarthelAdmissionTotalScoreStandard ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, boxwex=0.2, main="First prosthetic provision", xlab="Knee category", ylab="Barthel Index",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=16, outcol=addTrans(vcol,200))
boxplot(BarthelDischargeTotalScoreStandard ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="FirstDeliv",],
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=16, outcol=addTrans(vcol,200))
text(x=1:4, y=30, labels="T0", cex=1.5)
text(x=(1:4)+0.3, y=30, labels="T2", cex=1.5)
dev.off()
# boxplot - renewal
png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/Barthel_box_Renewal.png",
    width =7,height =6, units = 'in', res = 300)
boxplot(BarthelAdmissionTotalScoreStandard ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, boxwex=0.2, main="Prosthetic renewal", xlab="Knee category", ylab="Barthel Index",
        ylim=ylim, cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, pch=17, outcol=addTrans(vcol,200))
boxplot(BarthelDischargeTotalScoreStandard ~ KneeCategory, data=dfm[dfm$FirstdeliveryRenewal=="Renewal",], 
        col=vcol, add=T, boxwex=0.2, at=(1:4)+0.3,  xaxt='n',yaxt='n', pch=17, outcol=addTrans(vcol,200))
text(x=1:4, y=30, labels="T0", cex=1.5)
text(x=(1:4)+0.3, y=30, labels="T2", cex=1.5)
dev.off()
