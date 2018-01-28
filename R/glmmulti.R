# Function to run standard multivariable logistic regression ----
# Note, takes multiple explanatory AND dependent variables
# Dependent variables are run indiviudally, and models returned as a names list (setNames)
glmmulti <- function(df.in, dependent, explanatory, weights = NULL, family = "binomial"){
  result = list()
  if (is.null(weights)){
    for (i in 1:length(dependent)) {          
      f <- as.formula(paste(dependent, '~', paste(explanatory, collapse="+")))
      fit <- do.call("glm", list(formula=f, data=df.in, 
                                 family= family))
      fit['call'] = deparse(f)
      result[[i]] <- fit}
  } else {
    for (i in 1:length(dependent)) {          
      f <- as.formula(paste(dependent, '~', paste(explanatory, collapse="+")))
      fit <- do.call("glm", list(formula=f, data=df.in, 
                                 family= family, weights = weights))
      fit['call'] = deparse(f)
      result[[i]] <- fit}
  }
  result = setNames(result, dependent)
  class(result) = "glmlist"
  return(result)
}

