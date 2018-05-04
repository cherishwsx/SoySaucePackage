#' Determine the best splitting rule
#' @description  This function is used in \code{buildSubTree()}, it is used to find out the best splitting rule for the current data. In general, this function loops through each column of data(each predictor). For each column, if the values can be further split, then the function \code{find\_node()} is called to find out the splitting rule with the minimum entropy for that column. The resulting node positions and entropies for each column are stored in lists. For the column with the minimum entropy, a corresponding object of class ``Rule'' is constructed using \code{newRule()} and then returned.
#' @param myData Attribute data
#' @param classes Class data
#' @return An object of class ''Rule''
#' @export
#'
bestRule <- function(myData, classes, featureSpace) {
  errors <- rep(x = Inf, times = ncol(myData))
  values <- rep(x = NA, times = ncol(myData))
  for (thfeat in 1:ncol(myData)) {
    if (length(table(myData[,thfeat]))!=1) {
      subsplit <- find_node(features = myData[,thfeat], class = classes)
      errors[thfeat] <- subsplit[[2]]
      values[thfeat] <- subsplit[[1]]
    }
  }
  thisrule <- newRule(feature = colnames(myData)[which.min(errors)], judgement = "(<)=", value = values[which.min(errors)])
  if (errors[which.min(errors)]==Inf) thisrule$feature <- NA
  inde <- 2
  return(thisrule)
}
