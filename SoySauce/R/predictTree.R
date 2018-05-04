#' Predict the class of new data
#' @description This function is used in \code{randforePredict()}, it is designed to use decision tree models to predict the class of new data. Its inputs is a single-row new data and an object of decision tree model, and its output is character class. This function has a recursive structure. It firstly set the tree as the current node, and then according to the splitting rule and data, the current node is set either to childT(when test data satisfy the splitting rule) or childF otherwise. The split rule would be NA at the leaves, the class of the corresponding node would be returned.
#' @param myData New test data
#' @param Tree The decision tree model
#' @return A character class
#' @export

predictTree <- function(myData, Tree) {
  myData <- as.data.frame(myData)
  thisNode <- Tree
  while (!is.na(thisNode$splitRule)) {
    if (is.numeric(myData[1,thisNode$splitRule$feature])) {
      if (myData[1,thisNode$splitRule$feature]<=thisNode$splitRule$value) {thisNode <- thisNode$childT} else {thisNode <- thisNode$childF}
    } else {
      if (myData[1,thisNode$splitRule$feature]==thisNode$splitRule$value) {thisNode <- thisNode$childT} else {thisNode <- thisNode$childF}
    }
  }
  return(thisNode$class)
}
