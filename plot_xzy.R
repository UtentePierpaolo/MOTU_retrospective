# plot_xzy

# x: varps ("FirstdeliveryRenewal", "TimeFromAmputation_logdays","AmputationCause","DrugAntidepressants","DrugAntiepileptics")
# z: KneeCategory
# y: NumberFallsWithProsthesis

# plot IR for y on groups defined by (x,z)

# iteration over varps hard coded


plot_xzy <- function(llIRxzy, saveaddress){
  
  require(Hmisc)
  
  vcol= c("dark green","dark orange","dark blue","darkorchid3","dark red")
  
  
  png(paste0(saveaddress, "Plot_xzy.png"),
      width =21,height =12, units = 'in', res = 300)
  
  # layout(matrix(1:6, 2, 3, by.row = T))
  op <- par(mfrow = c(2,3), cex = 1)
  

  # "FirstdeliveryRenewal"
  cpredIR <- llIRxzy[["FirstdeliveryRenewal"]]
  vxlev <- names(cpredIR)
  # plot
  plotEmpty("Reason for rehabilitation training")
  for (ixlev in 1:length(vxlev)){
    for (izlev in 1:length(vkc)){
      splotIR(IRestimates=cpredIR[[ixlev]][[izlev]], dx=0.1*(ixlev-1), col=vcol[ixlev])
    }
  }
  legend(x=0,y=20, legend=c("First prosth. provision","Prosth. renewal"),
         col=vcol[1:ixlev], lty=1, pch=16, lwd=2, cex=1.5)  
  
  
  # "TimeFromAmputation_c"
  cpredIR <- llIRxzy[["TimeFromAmputation_c"]]
  vxlev <- names(cpredIR)
  vxlev <- c("[  0.92, 33.2)","[ 33.25,275.25)","[275.25,899.8]")
  # plot
  plotEmpty("Time from amputation (months)")
  for (ixlev in 1:length(vxlev)){
    for (izlev in 1:length(vkc)){
      splotIR(IRestimates=cpredIR[[ixlev]][[izlev]], dx=0.1*(ixlev-1), col=vcol[ixlev])
    }
  }
  legend(x=0,y=20, legend=vxlev,
         col=vcol[1:ixlev], lty=1, pch=16, lwd=2, cex=1.5)  
  
  
  # "AmputationCause"
  cpredIR <- llIRxzy[["AmputationCause"]]
  vxlev <- names(cpredIR)
  # plot
  plotEmpty("Amputation cause")
  for (ixlev in 1:length(vxlev)){
    for (izlev in 1:length(vkc)){
      splotIR(IRestimates=cpredIR[[ixlev]][[izlev]], dx=0.1*(ixlev-1), col=vcol[ixlev])
    }
  }
  legend(x=2.1,y=20, legend=c("Cancer","Congenital","Infectious","Traumatic","Vascular"),
         col=vcol[1:ixlev], lty=1, pch=16, lwd=2, cex=1.5)  
  
  
  # "DrugAntidepressants"
  cpredIR <- llIRxzy[["DrugAntidepressants"]]
  vxlev <- names(cpredIR)
  # plot
  plotEmpty("Use of antidepressants")
  for (ixlev in 1:length(vxlev)){
    for (izlev in 1:length(vkc)){
      splotIR(IRestimates=cpredIR[[ixlev]][[izlev]], dx=0.1*(ixlev-1), col=vcol[ixlev])
    }
  }
  legend(x=0,y=20, legend=c("No use of antidep.","Use of antidep."),
         col=vcol[1:ixlev], lty=1, pch=16, lwd=2, cex=1.5)  
  
  
  # "DrugAntiepileptics"
  cpredIR <- llIRxzy[["DrugAntiepileptics"]]
  vxlev <- names(cpredIR)
  # plot
  plotEmpty("Use of antiepileptics")
  for (ixlev in 1:length(vxlev)){
    for (izlev in 1:length(vkc)){
      splotIR(IRestimates=cpredIR[[ixlev]][[izlev]], dx=0.1*(ixlev-1), col=vcol[ixlev])
    }
  }
  legend(x=0,y=20, legend=c("No use of antiepil.","Use of antiepil."),
         col=vcol[1:ixlev], lty=1, pch=16, lwd=2, cex=1.5)  
  
  dev.off()
}


# prepare empty plot
plotEmpty <- function(main){
  
  x <- 0:(length(vkc)-1) # 0:3 
  plot(0, type="n", ylim=c(0,20), xlim=c(0, 3.8), xaxt='n', 
       xlab="Knee category", ylab="Number of falls per 1000 days", 
       cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, main=main)
  axis(side = 1, at = x, labels=vkc, cex.lab=1.5, cex.axis=1.5)
}

splotIR <- function(IRestimates, dx, col){
  
  require(Hmisc)
  
  x <- 0:(length(vkc)-1) # 
  names(x) <- vkc
  x <- x[as.character(IRestimates$KneeCategory)]
  
  errbar(x+dx, IRestimates$irr, IRestimates$irr_u, IRestimates$irr_l,
         add=T,lwd=2, errbar.col=col, col=col)
}

