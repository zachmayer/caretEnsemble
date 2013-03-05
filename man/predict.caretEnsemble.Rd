\name{predict.caretEnsemble}
\alias{predict.caretEnsemble}
\title{Make predictions from a caretEnsemble. This function passes the data to each function in
turn to make a matrix of predictions, and then multiplies that matrix by the vector of
weights to get a single, combined vector of predictions.}
\usage{
  predict.caretEnsemble(ensemble, ...)
}
\arguments{
  \item{ensemble}{A caretEnsemble to make predictions
  from.}

  \item{...}{arguments (including newdata) to pass to
  predict.train.}
}
\description{
  Make predictions from a caretEnsemble. This function
  passes the data to each function in turn to make a matrix
  of predictions, and then multiplies that matrix by the
  vector of weights to get a single, combined vector of
  predictions.
}

