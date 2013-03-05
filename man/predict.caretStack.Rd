\name{predict.caretStack}
\alias{predict.caretStack}
\title{Make predictions from a caretStack. This function passes the data to each function in
turn to make a matrix of predictions...}
\usage{
  predict.caretStack(stack, ...)
}
\arguments{
  \item{stack}{A caretStack to make predictions from.}

  \item{...}{arguments (including newdata) to pass to
  predict.train. DO NOT SPECIFY the "type" argument.}
}
\description{
  Make predictions from a caretStack. This function passes
  the data to each function in turn to make a matrix of
  predictions...
}

