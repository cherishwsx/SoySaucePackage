#' Generate a forest
#' @description This function is used to generate multiple decision tree. A technique called Bagging is applied to reduce variance and increase the prediction accuracy (James, Witten, Hastie, \&Tibshirani, 2017). Specifically, the number of features used in bootstrapped training set is determined to be
#' \deqn{m=\sqrt{p}} This function takes four inputs: feature, class, NumofBag and NumofObs. NumofBag is the number of bootstrapped training set needed to generate and NumofObs the the number of observation per training set. Given these two inputs, bootstrapped features set and class set are sampled from original features section and class section. With the generated bootstrapped set, \code{buildSubTree()} is called to generate the single decision tree. Accordingly, an aggregated forest list contained multiple tree list is returned.
#' @inheritParams get_entropy
#' @param NumofBag The number of tree in the forest
#' @param NumofObs The number of observation in a single tree
#' @return A list of multiple tree lists.
#' @export

get_Forest <- function(feature, class, NumofBag, NumofObs) {
  FeaturePerTree <- round(sqrt(length(feature)))
  ForestList <- rep(list(0),NumofBag)

  for (ii in 1:NumofBag) {
    if (NumofObs > nrow(feature)) {
      warning('Number of observation per sample is greater than the total sample size in data')
    } else {
      ObsInd <- sample(1:nrow(feature), NumofObs, replace = T)
    }

    FeatureInd <- sample(1:length(feature), FeaturePerTree)

    subFeature <- feature[ObsInd,]
    subClass <- class[ObsInd]

    ForestList[[ii]] <- buildSubTree(myData=subFeature, classes = subClass)
  }
  return(ForestList)
}
