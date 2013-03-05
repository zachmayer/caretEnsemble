\name{multiPredict}
\alias{multiPredict}
\title{Make a matrix of predictions from a list of caret models}
\usage{
  multiPredict(list_of_models, type, ...)
}
\arguments{
  \item{list_of_models}{a list of caret models to make
  predictions for}

  \item{type}{Classification or Regression}

  \item{...}{additional arguments to pass to predict.train.
  DO NOT PASS the "type" argument.}
}
\description{
  Make a matrix of predictions from a list of caret models
}

