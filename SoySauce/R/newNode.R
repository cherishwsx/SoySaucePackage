#'Build node information
#' @description Each node in the decision tree is built using this function. There are two types of nodes created in general. One type of nodes is intermediate nodes(or branches). They have attributes splitRule which store information about how data is split in the child nodes, and childT and childF as the corresponding child nodes depend on whether they satisfy the criteria of splitting rule. Another type of nodes serve as leaves of the tree(so called terminal nodes). They do not have information for the attributes listed above, rather, they have information class which store the assigned class of that leaf. In this function, the four arguments are stored in a single list, and the list is of class ``Node''.
#' @param splitRule
#' @param class
#' @param childT
#' @param childF
#' @return
#' @export

newNode <- function(splitRule, class, childT, childF) {
  thisNode <- list(splitRule, class, childT, childF)
  names(thisNode) <- c("splitRule", "class", "childT", "childF")
  class(thisNode) <- "Node"
  return(thisNode)
}
