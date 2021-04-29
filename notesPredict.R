# notesPredict

rm(list=ls())
gc()

setwd("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Code/MOTU_retrospective/") 

load("results.RData")

newdata <- data.frame(LengthOfStay=1, KneeCategory=factor(c("LK","AMK","FK","MPK")))
plink <- predict(llmodelszy$ates0, newdata=newdata)
ir <- exp(coef(plink) + log(1000))
irci <- exp(confint(plink, level=0.95) + log(1000))
irci_PSweighted <- data.frame(irr=ir, irr_l=irci[,1], irr_u=irci[,2])
plotIR(data.estimates=irci_PSweighted, main="Falls with prosthesis - PS weighted")


newdata <- data.frame(LengthOfStay=1000, KneeCategory=factor(c("LK","AMK","FK","MPK")))
irci_unweighted <- fdrr_mp(gmem=llmodelszy$lmodel0$NumberFallsWithProsthesis, 
                          newdata=newdata, CIlevel=0.95)
plotIR(data.estimates=irci_unweighted, main="Falls with prosthesis")
