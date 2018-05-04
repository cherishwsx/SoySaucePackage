#' Generate nodes of a decision tree
#' @description This is a recursive function which generates nodes of a decision tree. It is called in \code{get_Forest()} and in the function itself. It takes in several arguments, including the data and the current depth of the node. This function firstly calls \code{stopOrNot()}. If the returned value is true, then it means the terminal condition of building nodes is met, and it would create and return a node whose class is set to the majority class of data assigned to the node, using function \code{newNode()}. Next, \code{buildSubTree()} calls \code{bestRule()}, the returned object is of class ``Rule'', and it stores information of way to split the given data that can result in the minimum entropy. The returned rule is then stored in variable rulefound. Note that the function \code{bestRule()} sometimes return NA. This is because the data cannot be further split depend on the given data (for example, the features are all categorical, and features of the given data are identical). In this case a terminal node is returned. If the terminal condition if not met, and if there is a usable rule to split the data, then the data is split into two groups subData1 and subData2 according to the best rule. Each of the sub-groups is then passed as argument to \code{buildSubTree()} in order to obtain child nodes. At last, a node is built using \code{newNode()} with rulefound as split rule and returned objects of \code{buildSubTree()} as child nodes.
#' @inheritParams stopOrNot
#' @param classes Class data
#' @param featureSpace
#' @param random Do you want to the features data to be sample randomly when creating the tree?
#' @return A list that contains a tree information
#' @export

buildSubTree <- function(myData, classes, depth = 1, featureSpace = list(NA), random = T) {
  depthsu <- depth
  if (stopOrNot(myData = myData, depthsu)) {
    return(newNode(splitRule = NA, class =paste(as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1])), childT = NA, childF = NA))} else {
      if (random) {rulefound <- bestRule(myData = myData[,sample(1:length(myData),round(sqrt(length(myData))))], classes = classes, featureSpace = featureSpace)} else {
        rulefound <- bestRule(myData = myData, classes = classes, featureSpace = featureSpace)}
      if (is.na(rulefound$feature)) {return(newNode(splitRule = NA, class =paste(as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1])), childT = NA, childF = NA))}
      featureSpace <- list.append(featureSpace, rulefound)
      if (is.numeric(myData[1,rulefound$feature])) {
        subData1 <- myData[(myData[,rulefound$feature]<=rulefound$value),]
        subData2 <- myData[!(myData[,rulefound$feature]<=rulefound$value),]
        subClass1 <- classes[(myData[,rulefound$feature]<=rulefound$value)]
        subClass2 <- classes[!(myData[,rulefound$feature]<=rulefound$value)]
      } else {
        subData1 <- myData[(myData[,rulefound$feature]==rulefound$value),]
        subData2 <- myData[!(myData[,rulefound$feature]==rulefound$value),]
        subClass1 <- classes[(myData[,rulefound$feature]==rulefound$value)]
        subClass2 <- classes[!(myData[,rulefound$feature]==rulefound$value)]
      }
      if((nrow(subData1)==nrow(myData))||(nrow(subData2)==nrow(myData))) {return(newNode(splitRule = NA, class =paste(as.character(as.data.frame(sort(table(classes), decreasing = T))[1,1])), childT = NA, childF = NA))}
      depth <- depth + 1
      childT <- buildSubTree(myData = subData1, classes = subClass1, depth = depth, featureSpace = featureSpace, random = random)
      childF <- buildSubTree(myData = subData2, classes = subClass2, depth = depth, featureSpace = featureSpace, random = random)
      thisNode <- newNode(splitRule = rulefound, class = NA, childT = childT, childF = childF)
      return(thisNode)
    }
}
