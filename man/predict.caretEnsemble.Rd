\name{predict.caretEnsemble}
\alias{predict.caretEnsemble}
\title{Make predictions from a caretEnsemble. This function passes the data to each function in
turn to make a matrix of predictions, and then multiplies that matrix by the vector of
weights to get a single, combined vector of predictions.}
\usage{
\method{predict}{caretEnsemble}(object, keepNA = TRUE, se = NULL, ...)
}
\arguments{
  \item{object}{a \code{\link{caretEnsemble}} to make
  predictions from.}

  \item{keepNA}{a logical indicating whether predictions
  should be made for all cases where sufficient data exists
  or only for complete cases across all models. When TRUE
  this does not predict for missing values. When FALSE,
  missing values are overwritten with predictions where
  possible.}

  \item{se}{logical, should prediction errors be produced?
  Default is false.}

  \item{...}{arguments (including newdata) to pass to
  predict.train. These arguments must be named}
}
\description{
Make predictions from a caretEnsemble. This function passes
the data to each function in turn to make a matrix of
predictions, and then multiplies that matrix by the vector
of weights to get a single, combined vector of predictions.
}

