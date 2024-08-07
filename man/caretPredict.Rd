% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caretPredict.R
\name{caretPredict}
\alias{caretPredict}
\title{Prediction wrapper for \code{\link[caret]{train}}}
\usage{
caretPredict(object, newdata = NULL, excluded_class_id = 1L, ...)
}
\arguments{
\item{object}{a \code{\link[caret]{train}} object}

\item{newdata}{New data to use for predictions. If NULL, stacked predictions from the training data are returned.}

\item{excluded_class_id}{an integer indicating the class to exclude. If 0L, no class is excluded}

\item{...}{additional arguments to pass to \code{\link[caret]{predict.train}}, if newdata is not NULL}
}
\value{
a data.table
}
\description{
This is a prediction wrapper for \code{\link[caret]{train}} with several features:
- If newdata is null, return stacked predictions from the training job, rather than in-sample predictions.
- Always returns probabilities for classification models.
- Optionally drops one predicted class for classification models.
- Always returns a \code{\link[data.table]{data.table}}
}
\keyword{internal}
