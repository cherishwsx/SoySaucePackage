#' Calculate the single entropy
#' @description
#' To calculate entropy for single branch, the data is first separated into two sections. One is feature section which contains the attributes and the other one is class section which contains the classification result in the dataset. The two inputs in this function feature and class are corresponded to these sections. For each unique class existed in the class section, $-p_ilog_2p_i$ term is calculated and summed for all classes to obtain total initial single entropy.
#' @param features Attribute data
#' @param class class data
#' @return A numeric entropy value
#' @export

get_entropy<-function(features,class)
{
  total <- length(features)
  type <- rep(NA, length(unique(class)))
  prob <- rep(NA, length(unique(class)))
  one_ent <- rep(NA, length(unique(class)))

  for(ii in 1:length(unique(class))) {
    type[ii] <- sum(class==unique(class)[ii])
    prob[ii] <- type[ii] / total
    one_ent[ii] <- prob[ii]*(ifelse(prob[ii] > 0, log(prob[ii]), 0))
  }

  entropy <- -sum(one_ent)
  return(entropy)
}
