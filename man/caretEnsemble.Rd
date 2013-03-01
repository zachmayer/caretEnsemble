\name{caretEnsemble}
\alias{caretEnsemble}
\title{Find a good linear combination of several classification or regression models,
using either linear regression, elastic net regression, or greedy optimization.}
\usage{
  caretEnsemble(all.models, optFUN = greedOptRMSE, ...)
}
\arguments{
  \item{all.models}{a list of caret models to ensemble.}

  \item{optFUN}{the optimization function to use}

  \item{...}{additional arguments to pass to the
  optimization function}
}
\description{
  Find a good linear combination of several classification
  or regression models, using either linear regression,
  elastic net regression, or greedy optimization.
}

