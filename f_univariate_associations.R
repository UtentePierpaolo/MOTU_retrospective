# f_univariate_associations


# function to calculate a table with results of univariate association analyses
# either logistic regressions: f_univariate_associations_OR
# or poisson regression: f_univariate_associations_yRR

# f_univariate_associations_yRR already includes offset(log(LengthOfStay))



f_univariate_associations_OR <- function(y, xs, dfm){
  
  # y: name of dependent variable
  # xs: vector with names of independent variables to test
  # dfm: dataframe with data
  
  if(any(!(c(y, xs) %in% names(dfm)))){
    stop("variables not defined in the dataframe")
  }

  require(car)
  
  
  dfOR <- data.frame(variable=NA, level=NA, OR=NA, OR_lb=NA, OR_ub=NA, p=NA)
  inddf <- 1
  
  for(ind in 1:length(xs)){
    
    # as it is implemented now, when glmer outputs a warning, results are not written in dfOR
    
    tryCatch(
      expr = {
        print(ind)
        print(xs[ind])
        
        if (is.character(dfm[,xs[ind]])){
          dfm[,xs[ind]] <- factor(dfm[,xs[ind]])
        }
        
          
          cformula <- paste0(y, " ~ ", xs[ind])
          cdfm <- na.omit(dfm[,c(y,xs[ind])])
          m1 <- glm(cformula, data=cdfm, family = binomial(link = "logit")) #
          
        
        t1 <- summary(m1)$coefficients
        
        # Analysis of deviance table
        caov <- Anova(m1)
        
        for (indl in 2:nrow(t1)){
          inddf <- inddf + 1
          if (is.factor(cdfm[,xs[ind]])){
            dfOR[inddf,"level"] <- rownames(t1)[indl]
          }
          dfOR[inddf,"OR"] <- exp(t1[indl,"Estimate"])
          dfOR[inddf,"OR_lb"] <- exp(t1[indl,"Estimate"]-qnorm(1-0.05/2)*t1[indl,"Std. Error"])
          dfOR[inddf,"OR_ub"] <- exp(t1[indl,"Estimate"]+qnorm(1-0.05/2)*t1[indl,"Std. Error"])
          dfOR[inddf,"CI"] <- paste0("(",round(dfOR[inddf,"OR_lb"],digit=2),"-",round(dfOR[inddf,"OR_ub"],digit=2),")")
          if (indl==2){
            dfOR[inddf,"variable"] <- xs[ind]
            dfOR[inddf,"p"] <- caov$`Pr(>Chisq)`
          }
        }
        if (is.factor(cdfm[,xs[ind]])){
          inddf <- inddf + 1
          mcontr <- contrasts(cdfm[,xs[ind]])
          dfOR[inddf,"level"] <- names(rowSums(mcontr))[rowSums(mcontr)==0]
          dfOR[inddf,"OR"] <- 1
        }
        
      },
      error = function(e){ 
        message('Caught an error!')
        print(e)
      },
      warning = function(w){
        message('Caught a warning!')
        print(w)
      },
      finally = {
        # (Optional)
        # Do this at the end before quitting the tryCatch structure...
      }
    )
    
  }
  
  dfOR <- dfOR[2:nrow(dfOR),]

  return(dfOR)
}


f_univariate_associations_yRR <- function(y, xs, dfm){
  
  # y: name of dependent variable
  # xs: vector with names of independent variables to test
  # dfm: dataframe with data
  
  if(any(!(c(y, xs, "AnonymousID","LengthOfStay") %in% names(dfm)))){
    stop("variables not defined in the dataframe")
  }

  require(lme4)
  require(car)
  
  
  dfRR <- data.frame(variable=NA, level=NA, RR=NA, RR_lb=NA, RR_ub=NA, p=NA)
  inddf <- 1
  
  for(ind in 1:length(xs)){
    
    # as it is implemented now, when glmer outputs a warning, results are not written in dfRR
    
    tryCatch(
      expr = {
        print(ind)
        print(xs[ind])
        
        if (is.character(dfm[,xs[ind]])){
          dfm[,xs[ind]] <- factor(dfm[,xs[ind]])
        }
        
        cformula <- paste0(y, " ~ offset(log(LengthOfStay)) + ", xs[ind], " + (1 | AnonymousID)")
        cdfm <- na.omit(dfm[,c(y,"LengthOfStay",xs[ind],"AnonymousID")])
        m1 <- glmer(cformula, data=cdfm, family = poisson(link = "log"), nAGQ = 20)
        
        t1 <- summary(m1)$coefficients
        
        # likelihood ratio test (I think so)
        # clrt <- drop1(m1,test="Chisq")
        # vp[ind] <- clrt$`Pr(Chi)`[2]
        # Analysis of deviance table
        caov <- Anova(m1)
        
        for (indl in 2:nrow(t1)){
          inddf <- inddf + 1
          if (is.factor(cdfm[,xs[ind]])){
            dfRR[inddf,"level"] <- rownames(t1)[indl]
          }
          dfRR[inddf,"RR"] <- exp(t1[indl,"Estimate"])
          dfRR[inddf,"RR_lb"] <- exp(t1[indl,"Estimate"]-qnorm(1-0.05/2)*t1[indl,"Std. Error"])
          dfRR[inddf,"RR_ub"] <- exp(t1[indl,"Estimate"]+qnorm(1-0.05/2)*t1[indl,"Std. Error"])
          dfRR[inddf,"CI"] <- paste0("(",round(dfRR[inddf,"RR_lb"],digit=2),"-",round(dfRR[inddf,"RR_ub"],digit=2),")")
          if (indl==2){
            dfRR[inddf,"variable"] <- xs[ind]
            # dfRR[inddf,"p"] <- clrt$`Pr(Chi)`[2]
            dfRR[inddf,"p"] <- caov$`Pr(>Chisq)`
          }
        }
        if (is.factor(cdfm[,xs[ind]])){
          inddf <- inddf + 1
          mcontr <- contrasts(cdfm[,xs[ind]])
          dfRR[inddf,"level"] <- names(rowSums(mcontr))[rowSums(mcontr)==0]
          dfRR[inddf,"RR"] <- 1
        }
        
      },
      error = function(e){ 
        message('Caught an error!')
        print(e)
      },
      warning = function(w){
        message('Caught a warning!')
        print(w)
      },
      finally = {
        # (Optional)
        # Do this at the end before quitting the tryCatch structure...
      }
    )
    
  }
  
  dfRR <- dfRR[2:nrow(dfRR),]
  
  
  return(dfRR)
}