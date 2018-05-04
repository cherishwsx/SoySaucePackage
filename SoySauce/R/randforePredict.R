#' Make prediction with random forest
#' @description This function is used to make predictions based on the random forest model. It takes two inputs: list of built decision trees returned by \code{get\_Forest()} and test data as myData. This function returns a vector of predicted classes. For each row of the data given, a list of predictions is initialized and filled using function \code{predictTree()}. Each cell of the prediction list is then filled using the corresponding decision tree in the ForestList and data in the corresponding row. Since random forest usually takes majority vote for classification problems, so the most occurring class in the prediction list would be chosen as the predicted class for that row of data.
#' @param ForestList A List of multiple decision trees (Forest model)
#' @param myData Test data
#' @return A vector of predicted class
#' @export
#'


randforePredict <- function(ForestList, myData) {
  returnClass <- rep(x = NA, times = nrow(myData))
  for (j in 1:nrow(myData)) {
    prediction <- rep(x = NA, times = length(ForestList))
    for (i in 1:length(ForestList)) {
      prediction[i] <- predictTree(Tree =ForestList[[i]], myData = myData[j,])
    }
    if (length(unique(prediction))==1) {returnClass[j] <- unique(prediction)} else {
      returnClass[j] <- paste(as.character(as.data.frame(sort(table(prediction), decreasing = T))[1,1]))
    }
  }
  return(returnClass)
}
