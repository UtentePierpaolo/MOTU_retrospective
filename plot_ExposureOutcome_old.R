# plot_ExposureOutcome


# Plot unweighted and weighted incidence rates (IR) per proshetic knee category
# and pairwise comparisons


plot_ExposureOutcome <- function(modelszy){
  
  
  require(multcomp)
  source("responsescale_plotconfintglht.R")
  source("fdrrs.R")
  
  # unpackage parameters
  ates0 <- modelszy[["ates0"]]
  ates <- modelszy[["ates"]]
  ates_allc <- modelszy[["ates_allc"]]
  
  
  data.estimates <- fdrr_svy(gmem=ates0)
  
  lcontrasts <- list()
  mci <- ates_allc #Estimate        lwr        upr
  mci$lwr <- qt(p=0.025, df=summary(ates)$df.residual)*ates_allc$`Std. Error` + ates_allc$Estimate
  mci$upr <- qt(p=0.975, df=summary(ates)$df.residual)*ates_allc$`Std. Error` + ates_allc$Estimate
  lcontrasts$confint <- as.matrix(mci)
  
  x <- 0:3 # 
  
  # png("C:/Users/Pierpaolo/OneDrive - Alma Mater Studiorum Università di Bologna/MOTU/retrospective study/Figures/fall_KneeCategory_PSweighted.png",
  #   width =7,height =10, units = 'in', res = 300)
  
  layout(matrix(1:2, 2, 1))  
  
  plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3.2), xaxt='n', 
       xlab="Knee category", ylab="Number of falls per 1000 days", 
       cex.lab=1.1, cex.axis=1.1, cex.main=1.1, cex.sub=1.5, main="Falls with prosthesis - PS weighted")
  axis(side = 1, at = x, labels=data.estimates$KneeCategory, cex.lab=1.1, cex.axis=1.1)
  errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
         add=T,lwd=2, errbar.col="dark blue", col="dark blue")
  
  # pairwise significant differences
  # hard-coded
  # 3-2
  lines(c(2,2,3,3),c(3.5,3.7,3.7,3.5), lwd=1.5)
  text(x=2.5, y=3.85, label="*", cex=1.5)
  
  # paiwise comparisons
  
 
  fcg(lcontrasts, 
      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="95% confidence level")
   
  
  # dev.off()
}




