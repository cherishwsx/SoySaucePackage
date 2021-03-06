% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bestRule.R
\name{bestRule}
\alias{bestRule}
\title{Determine the best splitting rule}
\usage{
bestRule(myData, classes, featureSpace)
}
\arguments{
\item{myData}{Attribute data}

\item{classes}{Class data}
}
\value{
An object of class ''Rule''
}
\description{
This function is used in \code{buildSubTree()}, it is used to find out the best splitting rule for the current data. In general, this function loops through each column of data(each predictor). For each column, if the values can be further split, then the function \code{find\_node()} is called to find out the splitting rule with the minimum entropy for that column. The resulting node positions and entropies for each column are stored in lists. For the column with the minimum entropy, a corresponding object of class ``Rule'' is constructed using \code{newRule()} and then returned.
}
