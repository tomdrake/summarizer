#includes weights
glmuni <- function(df.in, dependent, explanatory, weights = NULL, family = "binomial"){
  result = list()
  if (is.null(weights)){
    for (i in 1:length(explanatory)) {          
      f <- as.formula(paste(dependent, '~', paste(explanatory[i], collapse="+")))
      fit <- do.call("glm", list(formula=f, data=df.in, 
                                 family= family))
      fit['call'] = deparse(f)
      result[[i]] <- fit}
  } else {
    for (i in 1:length(explanatory)) {          
      f <- as.formula(paste(dependent, '~', paste(explanatory[i], collapse="+")))
      fit <- do.call("glm", list(formula=f, data=df.in, 
                                 family= family, weights = weights))
      fit['call'] = deparse(f)
      result[[i]] <- fit}
  }
  result = setNames(result, dependent)
  class(result) = "glmlist"
  return(result)
}
