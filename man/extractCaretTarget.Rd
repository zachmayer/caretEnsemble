% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caretList.R
\name{extractCaretTarget}
\alias{extractCaretTarget}
\title{Extracts the target variable from a set of arguments headed to the caret::train function.}
\usage{
extractCaretTarget(...)
}
\arguments{
\item{...}{a set of arguments, as in the caret::train function}
}
\description{
This function extracts the y variable from a set of arguments headed to a caret::train model.
Since there are 2 methods to call caret::train, this function also has 2 methods.
}
\keyword{internal}
