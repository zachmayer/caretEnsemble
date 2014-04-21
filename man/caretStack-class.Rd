\docType{class}
\name{caretStack-class}
\alias{caretStack-class}
\title{Class "caretStack" of ensembled train objects from the caret package}
\usage{
caretStack(all.models, ...)
}
\description{
Class "caretStack" of ensembled train objects from the
caret package
}
\details{
The object has the following items \itemize{ \item{models -
a list of the original models to be ensembled}
\item{ens_model - a \code{\link{train}} object} \item{error
- the final accuracy metric of the ensembled models} }
}
\section{Objects from the Class}{
  Objects are created by calls to \code{\link{caretStack}}.
}
\examples{
showClass("caretEnsemble")
methods(class="caretEnsemble")
}
\seealso{
\code{\link{caretEnsemble}}
}
\keyword{classes}

