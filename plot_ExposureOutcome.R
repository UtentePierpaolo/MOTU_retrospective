# plot_ExposureOutcome


# Plot unweighted and weighted incidence rates (IR) per proshetic knee category
# and pairwise comparisons


plot_ExposureOutcome <- function(modelszy, saveaddress=NULL){
  
  require(multcomp)
  require(Hmisc)
  source("responsescale_plotconfintglht.R")
  source("fdrrs.R")
  
  # unpackage parameters
  # unadjusted models
  lmodel0 <- modelszy[["lmodel0"]]
  lmodel1 <- modelszy[["lmodel1"]]
  # PS-weighted models
  ates0 <- modelszy[["ates0"]]
  ates <- modelszy[["ates"]]
  ates_allc <- modelszy[["ates_allc"]]
  
  
  # unadjusted models---------------------
  
  data.estimates <- fdrr(gmem=lmodel0[["NumberFallsWithProsthesis"]])
  data.estimates <- data.estimates[paste0("KneeCategory",vkc),]
  
  x <- 0:3 # 
  
  if (!is.null(saveaddress)){
    png(paste0(saveaddress,"fall_KneeCategory.png"),
        width =7,height =10, units = 'in', res = 300)
  }
  
  layout(matrix(1:2, 2, 1))  
  
  plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3.2), xaxt='n', 
       xlab="Knee category", ylab="Number of falls per 1000 days", 
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="Falls with prosthesis")
  axis(side = 1, at = x, labels=vkc, cex.lab=1.5, cex.axis=1.5)
  errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
         add=T,lwd=2, errbar.col="dark blue", col="dark blue")
  
  # pairwise significant differences
  # hard-coded
  # 2-0
  lines(c(0,0,2,2),c(3,3.2,3.2,3), lwd=1.5)
  text(x=1, y=3.35, label="*", cex=1.5)
  # 3-2
  lines(c(2,2,3,3),c(3.5,3.7,3.7,3.5), lwd=1.5)
  text(x=2.5, y=3.85, label="**", cex=1.5)
  
  # paiwise comparisons
  
  wht <- glht(lmodel1[["NumberFallsWithProsthesis"]], linfct = mcp(KneeCategory = c("AMK - LK = 0", "FK - LK = 0", "MPK - LK = 0", "FK - AMK = 0", "MPK - AMK = 0", "MPK - FK = 0") ))
  
  fcg(confint(wht), 
      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, 
      main="95% confidence interval")

  if (!is.null(saveaddress)){
    dev.off()
  }
  
   
 
   # PS-weighted model----------------------

   data.estimates <- fdrr_svy(gmem=ates0)
   
   lcontrasts <- list()
   mci <- ates_allc #Estimate        lwr        upr
   mci$lwr <- qt(p=0.025, df=summary(ates)$df.residual)*ates_allc$`Std. Error` + ates_allc$Estimate
   mci$upr <- qt(p=0.975, df=summary(ates)$df.residual)*ates_allc$`Std. Error` + ates_allc$Estimate
   lcontrasts$confint <- as.matrix(mci)
   
   x <- 0:3 # 
   
   if (!is.null(saveaddress)){
     png(paste0(saveaddress,"fall_KneeCategory_PSweighted.png"),
         width =7,height =10, units = 'in', res = 300) 
   }
   
  layout(matrix(1:2, 2, 1))  
   
   plot(x, data.estimates$irr, ylim=c(0,5), xlim=c(0, 3.2), xaxt='n', 
        xlab="Knee category", ylab="Number of falls per 1000 days", 
        cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main="Falls with prosthesis - PS weighted")
   axis(side = 1, at = x, labels=data.estimates$KneeCategory, cex.lab=1.5, cex.axis=1.5)
   errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
          add=T,lwd=2, errbar.col="dark blue", col="dark blue")
   
   # pairwise significant differences
   # hard-coded
   # 3-2
   lines(c(2,2,3,3),c(3.5,3.7,3.7,3.5), lwd=1.5)
   text(x=2.5, y=3.85, label="*", cex=1.5)
   
   # paiwise comparisons
   
   
   fcg(lcontrasts, 
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, 
       main="95% confidence interval")
   
   if (!is.null(saveaddress)){
     dev.off()
   }
   
  }
