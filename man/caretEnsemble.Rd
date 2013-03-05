\name{caretEnsemble}
\alias{caretEnsemble}
\title{Combine several predictive models via weights}
\usage{
  caretEnsemble(all.models, optFUN = greedOptRMSE, ...)
}
\arguments{
  \item{all.models}{a list of caret models to ensemble.}

  \item{optFUN}{the optimization function to use}

  \item{...}{additional arguments to pass to the
  optimization function}
}
\value{
  S3 caretEnsemble object
}
\description{
  Find a good linear combination of several classification
  or regression models, using either linear regression,
  elastic net regression, or greedy optimization.
}
\details{
  Every model in the "library" must be a separate
  \code{train} object.  For example, if you wish to combine
  a random forests with several different values of mtry,
  you must build a model for each value of mtry.  If you
  use several values of mtry in one train model, (e.g.
  tuneGrid = expand.grid(.mtry=2:5)), caret will select the
  best value of mtry before we get a chance to include it
  in the ensemble.
}
\references{
  \url{http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.60.2859&rep=rep1&type=pdf}
}

