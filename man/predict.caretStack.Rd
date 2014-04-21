\name{predict.caretStack}
\alias{predict.caretStack}
\title{Make predictions from a caretStack}
\usage{
\method{predict}{caretStack}(ensemble, newdata = NULL, ...)
}
\arguments{
  \item{ensemble}{a \code{\linkS4class{caretStack}} to make
  predictions from.}

  \item{...}{arguments (including newdata) to pass to
  predict.train.}
}
\description{
Make predictions from a caretStack. This function passes
the data to each function in turn to make a matrix of
predictions, and then multiplies that matrix by the vector
of weights to get a single, combined vector of predictions.
}

