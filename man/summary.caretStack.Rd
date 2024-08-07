% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caretStack.R
\name{summary.caretStack}
\alias{summary.caretStack}
\title{Summarize a caretStack object}
\usage{
\method{summary}{caretStack}(object, ...)
}
\arguments{
\item{object}{An object of class caretStack}

\item{...}{ignored}
}
\description{
This is a function to summarize a caretStack.
}
\examples{
models <- caretList(
  x = iris[1:100, 1:2],
  y = iris[1:100, 3],
  methodList = c("rpart", "glm")
)
meta_model <- caretStack(models, method = "lm")
summary(meta_model)
}
