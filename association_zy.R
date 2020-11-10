# association_zy


# association analyses between knee category (z) and falls (y)
# unadjusted and with propensity score weighting


association_zy <- function(){
  
  
  
  require(survey)
  dsd <- svydesign(id= ~1, weights= ~w0, data=dfprop)
  ates <- svyglm(suf12 ~ metcbt5 + scy, design=dsd)
  
}