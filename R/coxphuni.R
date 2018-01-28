# Survival
coxphuni <- function(df.in, dependent, explanatory, weights = NULL){
  require(survival)
  result <- list()
  if (is.null(weights)){
    for (i in 1:length(explanatory)){
      result[[i]] <- coxph(as.formula(paste0(dependent, "~", explanatory[i])), data=df.in)
    } 
  }else{
    for (i in 1:length(explanatory)){
      result[[i]] <- coxph(as.formula(paste0(dependent, "~", explanatory[i])), weights = weights, data=df.in)
    }
  }
  class(result) = "coxphlist"
  return(result)
}
