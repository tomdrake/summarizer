#Survival multivariable with weights - clusters also possible
coxphmulti <- function(df.in, dependent, explanatory, weights = NULL){
  require(survival)
  result = list()
  if (is.null(weights)){
  for (i in 1:length(dependent)){
    result[[i]] = coxph(as.formula(paste0(dependent, "~", paste(explanatory, collapse="+"))), data=df.in)
  }
  }else{
    for (i in 1:length(dependent)){
      result[[i]] = coxph(as.formula(paste0(dependent, "~", paste(explanatory, collapse="+"))), weights = weights, data=df.in)
    }
  }
  result = setNames(result, dependent)
  class(result) = "coxphlist"
  return(result)
}

