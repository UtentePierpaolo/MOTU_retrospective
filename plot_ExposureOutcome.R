# plot_ExposureOutcome


# Plot unweighted and weighted incidence rates (IR) per proshetic knee category
# and pairwise comparisons


plot_ExposureOutcome <- function(linference, lIR, saveaddress=NULL){
  
  # require(multcomp)
  source("responsescale_plotconfintglht.R")
  
  
  # unadjusted models---------------------
  
  
  data.estimates <- lIR$irci_unw[paste0("KneeCategory",vkc),]
  
  if (!is.null(saveaddress)){
    png(paste0(saveaddress,"fall_KneeCategory.png"), 
        width =7,height =10, units = 'in', res = 300)
  }
  
  layout(matrix(1:2, 2, 1))
  
  plotIR(data.estimates=lIR$irci_unw, main="Falls with prosthesis")
  
  # pairwise significant differences
  # hard-coded
  # 2-0
  lines(c(0,0,2,2),c(5.9,6.1,6.1,5.9), lwd=1.5)
  text(x=1, y=6.25, label="*", cex=1.5)
  # 3-2
  lines(c(2,2,3,3),c(6.4,6.6,6.6,6.4), lwd=1.5)
  text(x=2.5, y=6.75, label="**", cex=1.5)
  
  # paiwise comparisons
  fcg(linference$pcomp_unw, 
      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, 
      main="95% confidence interval")
  
  if (!is.null(saveaddress)){
    dev.off()
  }
  
  
  # PS-weighted model----------------------
  
  data.estimates <- lIR$irci_w
  
  lcontrasts <- linference$pcomp_w
  
  
  if (!is.null(saveaddress)){
    png(paste0(saveaddress,"fall_KneeCategory_PSweighted.png"), 
        width =7,height =10, units = 'in', res = 300)
  }
  
  layout(matrix(1:2, 2, 1))
  
  # incidence rates - PS weighted 
  plotIR(data.estimates=data.estimates, main="Falls with prosthesis - PS weighted")
  
  # pairwise significant differences
  # hard-coded
  # 3-2
  lines(c(2,2,3,3),c(4.5,4.7,4.7,4.5), lwd=1.5)
  text(x=2.5, y=4.85, label="*", cex=1.5)
  
  # paiwise comparisons
  fcg(lcontrasts, 
      cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, 
      main="95% confidence interval")
  
  if (!is.null(saveaddress)){
    dev.off()
  }
  
}



plotIR <- function(data.estimates, main){
  
  require(Hmisc)
  
  x <- 0:3 # 
  
  plot(x, data.estimates$irr, ylim=c(0,6.8), xlim=c(0, 3.2), xaxt='n', 
       xlab="Knee category", ylab="Number of falls per 1000 days", 
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main=main)
  axis(side = 1, at = x, labels=data.estimates$KneeCategory, cex.lab=1.5, cex.axis=1.5)
  errbar(x, data.estimates$irr, data.estimates$irr_u, data.estimates$irr_l,
         add=T,lwd=2, errbar.col="dark blue", col="dark blue")
  
}


