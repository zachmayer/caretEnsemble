% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caretPredict.R
\name{isClassifierAndValidate}
\alias{isClassifierAndValidate}
\title{Validate a model type}
\usage{
isClassifierAndValidate(object, validate_for_stacking = TRUE)
}
\arguments{
\item{object}{a \code{\link[caret]{train}} object}

\item{validate_for_stacking}{a logical indicating whether to validate the object for stacked predictions}
}
\value{
a logical. TRUE if classifier, otherwise FALSE.
}
\description{
Validate the model type from a \code{\link[caret]{train}} object.
For classification, validates that the model can predict probabilities, and,
 if stacked predictions are requested, that classProbs = TRUE.
}
\keyword{internal}
