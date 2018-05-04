#' Find the best node with minimum entropy
#' @description
#' This function is to determine the best node to split data. The criteria of best node is to have minimum split entropy since less entropy indicates more homogeneity in the group. Among all the possible splitting results, \code{get_split_entropy} is called within a loop in this function and returned the best node as well as the corresponding minimum entropy.
#' @inheritParams get_entropy
#' @return A list contains the best node position and its entropy

find_node<-function(features, class) {
  node <- 1
  min_ent <- get_split_entropy(features, class, node)

  if(is.numeric(features)) {
    for(ii in 1:length(features)) {
      ent <- get_split_entropy(features, class, ii)
      if(ent <= min_ent) {
        min_ent <- ent
        node <- ii
        result <- features[node]
      }
    }
  } else {
    features <- as.character(features)
    for (ii in 1:length(unique(features))) {
      ent <- get_split_entropy(features, class, ii)
      if(ent <= min_ent) {
        min_ent <- ent
        node <- ii
        result <- unique(features)[node]
      }
    }
  }
  return(list(node=result, Entropy=min_ent))
}
