#' To determine whether to stop building a tree or not
#' @description  When building a single decision tree, it is necessary to set parameters to determine when to stop splitting or generating nodes, otherwise the generating process would go on forever. This function can also be used to tune the variance and bias of the decision tree model. \code{stopOrNot()} is later used in \code{buildSubTree()}, and it would return a boolean variable according to the data and the values of minDataPoints and maxDepth. If one of minDataPoints and maxDepth is set to -1, then that variable would be ignored in building the decision tree. This function would return true if number of rows of data assigned to the node is less than or equal to minDataPoints, or the cumulative depth is larger than or equal to maxDepth. In Random Forest, single tree with large variance is favored, so the corresponding values of minDataPoints and maxDepth are set to 5 and -1.
#' @param myData Attribute data
#' @param depth The total number of layer for decision tree
#' @return A boolean indicating wether to stop or not
#' @export
#'
stopOrNot <- function(myData, depth) {
  minDataPoints <- 5
  maxDepth <- -1
  result <- F
  if ((minDataPoints != -1) && (nrow(myData) <= minDataPoints)) result <- T
  if ((maxDepth != -1) && (depth >= maxDepth)) result <- T
  return(result)
}
