#helper files
checkPlpObject <- function(x){
  if(sum(names(x)%in%c("performanceEvaluation","inputSetting","executionSummary",
                   "model","analysisRef","covariateSummary" )) == 6){
    return(T)
  }
  return(F)
}

checkPrediction <- function(x){
  if(sum(names(x)%in%c("prediction" )) == 1){
    return(T)
  }
  return(F)
}
