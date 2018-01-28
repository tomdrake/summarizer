# Function multiple univariable logistic regressions against a list of explanatory variables -----------
glmuni <- function(df.in, dependent, explanatory, weights = NULL){
  result <- list()
  if (is.null(weights)){
  for (i in 1:length(explanatory)){
    result[[i]] <- glm(paste(dependent, "~", explanatory[i]), data=df.in, family="binomial")
    }
    } else {
      for (i in 1:length(explanatory)){
      result[[i]] <- glm(as.formula(paste0(dependent, "~", explanatory[i])), weights = weights, data=df.in)
    }
  }
  class(result) = "glmlist"
  return(result)
}
