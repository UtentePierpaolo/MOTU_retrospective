# IRxzy



IRxzy <- function(dfm=dfr){
  
  
    require(lme4)
    require(Hmisc)
    source("fdrrs.R")
    
    nmin <- 5 # minimum number of falls per KneeCategory
    ndays <- 1000 # number of patient-days to express IRs
    CIlevel = 0.95
    
    calcPredIR <- function(cx){
      
      vxlev <- levels(dfm[,cx])
      predIR <- list()
      
      for (cxlev in vxlev){
        
        print(cxlev)
        predIR[[cxlev]] <- list()
        cformula1 <- "NumberFallsWithProsthesis  ~ offset(log(LengthOfStay)) + (1|AnonymousID)"
        
        for (czlev in vkc){
          
          print(czlev)
          cdfxz <- dfm[(dfm[,cx] %in% cxlev) & ((dfm[,"KneeCategory"] %in% czlev)),
                       c("LengthOfStay","NumberFallsWithProsthesis","KneeCategory","AnonymousID")]
          
          
          if(nrow(cdfxz)==0){
            
            predIR[[cxlev]][[czlev]] <- data.frame(irr=NA, irr_l=NA, irr_u=NA, KneeCategory=czlev)
            
          } else if(sum(cdfxz$NumberFallsWithProsthesis)<nmin){
            
            nfallwp <- sum(cdfxz$NumberFallsWithProsthesis)
            npatientdays <- sum(cdfxz$LengthOfStay)
            ir <- nfallwp/npatientdays * ndays
            predIR[[cxlev]][[czlev]] <- data.frame(irr=ir, irr_l=NA, irr_u=NA, KneeCategory=czlev)
            
          } else{
            
            mod <- glmer(cformula1, data=cdfxz, family = poisson(link = "log"), nAGQ = 20)
            newdata <- data.frame(LengthOfStay=1000, KneeCategory=czlev)
            predIR[[cxlev]][[czlev]] <- fdrr_mp(gmem=mod, newdata=newdata, CIlevel=CIlevel)
          }
        }
      }
      return(predIR)
    }
    
    
    # pre-processing
    # categorize "TimeFromAmputation_logdays" in tertiles
    dfm$TimeFromAmputation_c <- cut2(dfm$TimeFromAmputation_months, g=3)
    dfm$DrugAntidepressants <- factor(dfm$DrugAntidepressants)
    dfm$DrugAntiepileptics <- factor(dfm$DrugAntiepileptics)
    
    lpredIR <- list()
    
    cx="FirstdeliveryRenewal"
    lpredIR[[cx]] <- calcPredIR(cx)
    
    cx <- "TimeFromAmputation_c"
    lpredIR[[cx]] <- calcPredIR(cx)
    
    cx <- "AmputationCause"
    lpredIR[[cx]] <- calcPredIR(cx)
    
    cx <- "DrugAntidepressants"
    lpredIR[[cx]] <- calcPredIR(cx)
    
    cx <- "DrugAntiepileptics"
    lpredIR[[cx]] <- calcPredIR(cx)
    
  
  return(lpredIR)
}