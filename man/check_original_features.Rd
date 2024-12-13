% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caretStack.R
\name{check_original_features}
\alias{check_original_features}
\title{Check original_features parameter}
\usage{
check_original_features(original_features, newdata, training = TRUE)
}
\arguments{
\item{original_features}{a character vector with the names of the original features to include in the stack}

\item{newdata}{the data to use for train a model or make predictions}

\item{training}{a logical indicating whether the function is being called during training or prediction}
}
\description{
Check the original_features parameter for caretStack, predict.caretStack and varImp.caretStack
}
\keyword{internal}
