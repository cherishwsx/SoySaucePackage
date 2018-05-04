#' Creat a new Rule class
#' @description This function return an object of class ''Rule''. It takes in three arguments: feature, judgement and value. Feature stores a name of the variable to be split, judgment states how to split the data according the value. In this case, judgment is always set to the character ``($<$)=``. When the variable is categorical, judgement should be seen as ``=``, otherwise it is viewed as ``$<$=``. Lastly, value stores the categorical or numeric value of the rule. In this function, the three arguments are stored in a single list, and the list is of class ``Rule''.
#' @param feature The name of the variable to be split
#' @param judgement Comparison operator
#' @param value The number that is used to compare
#' @return An object of class ''Rule''

newRule <- function(feature, judgement, value) {
  thisRule <- list(feature, judgement, value)
  names(thisRule) <- c("feature", "judgement", "value")
  class(thisRule) <- "Rule"
  return(thisRule)
}
