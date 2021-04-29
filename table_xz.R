# table_xz



table_xz <- function(dfr, w=NULL, dfvarx, varz){
  
  # varx thought to contain names of personal risk factors
  # varz thought to contain "KneeCategory", which must be a factor within dfr
  
  # check
  if(any(!(c(varx, varz) %in% names(dfr)))){
    stop("some variables not defined in the dataframe")
  }

  levz <- levels(dfr[,varz])
  
  if(is.null(w)){
    dfr$w <- 1
  }else{
    dfr$w <- dfr[,w]
  }
  
  lStat <- list()
  for (indz in 1:length(levz)){
    
    wcz <- which(dfr[,varz] %in% levz[indz])
    dfrcz <- dfr[wcz,]
    
    dfStati <- table_xzi(dfr=dfrcz, dfvarx=dfvarx)
    
    lStat[[indz]] <- dfStati
  }
  names(lStat) <- levz
 
  # merge data.frame in lStat 
  # for(inds in 1:length())
  
  return(lStat) 
}  


table_xzi <- function(dfr, dfvarx){
  
  
  dfStati <- data.frame(variable=NA, level=NA, type=NA, Mean=NA, Sd=NA, N=NA, Perc=NA)
  inddf <- 1
  
  for(ind in 1:nrow(dfvarx)){
    
    cx <- as.character(dfvarx[ind,"varx"])
    
    dfStati[inddf,"variable"] <- cx
    
    if (dfvarx[ind,"type"] %in% c("categorical","dichotomous")){
      
      dfr[,cx] <- factor(dfr[,cx])
      levcx <- levels(dfr[,cx])
      
      for(indl in 1:length(levcx)){
        dfStati[inddf,"type"] <- "cat/dich"
        dfStati[inddf,"level"] <- levcx[indl]
        dfStati[inddf,"N"] <- sum(dfr[,cx] %in% levcx[indl])
        dfStati[inddf,"Perc"] <- sum(dfr$w * (dfr[,cx] %in% levcx[indl]))/sum(dfr$w * (!is.na(dfr[,cx])))
        inddf <- inddf+1
      }
      
    }else if (dfvarx[ind,"type"]=="continuous"){
      
      dfStati[inddf,"type"] <- "cont"
      xm <- weighted.mean(x=dfr[,cx], w=dfr$w, na.rm=T)
      dfStati[inddf,"Mean"] <- xm
      
      if(min(dfr$w)==max(dfr$w)){
        dfStati[inddf,"Sd"] <- sd(dfr[,cx], na.rm=T)
      }else{
        w <- (dfr$w)/sum(dfr$w)
        dfStati[inddf,"Sd"] <- sqrt(sum(w * (dfr[,cx] - xm)^2, na.rm=T))
      }
      
      inddf <- inddf+1
    }
  }
  
  return(dfStati)
}  
  
  


  
print_Tabxz <- function(lStat){
  
  lStat_print <- list()
  for(indl in 1:length(lStat)){
    cStat <- lStat[[indl]]
    cStat$Mean <- round(cStat$Mean, digit=1)
    cStat$Sd <- round(cStat$Sd, digit=1)
    cStat$Mean_Sd <- ifelse(cStat$type=="cont", paste0(cStat$Mean," (",cStat$Sd,")"), NA)
    cStat$Perc <- paste0(round(cStat$Perc, digits=3)*100,"%") 
    cStat$N_Perc <- ifelse(cStat$type=="cat/dich", paste0(cStat$N," (",cStat$Perc,")"),NA)
    lStat_print[[indl]] <- cStat[,c("variable","level","Mean_Sd","N_Perc")]
    # lStat_print[[indl]] <- cStat[,c("variable","level","Mean_Sd","Perc")]
  }
  names(lStat_print) <- names(lStat)
  return(lStat_print)
}  



