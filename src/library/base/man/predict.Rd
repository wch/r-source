\name{predict}
\title{Model Predictions}
\usage{
predict (object, \dots)
}
\alias{predict}
\arguments{
  \item{object}{a model object for which prediction is desired.}
  \item{\dots}{additional arguments affecting the predictions produced.}
}
\description{
  \code{predict} is a generic function for predictions from the results of
  various model fitting functions.  The function invokes particular
  \emph{methods} which depend on the \code{\link[base]{class}} of
  the first argument.

  The function \code{\link{predict.lm}} makes predictions based on the
  results produced by \code{\link{lm}}.
}
\value{
  The form of the value returned by \code{predict} depends on the
  class of its argument.  See the documentation of the
  particular methods for details of what is produced by that method.
}
\seealso{\code{\link{predict.lm}}.
}
\examples{
## All the "predict" methods visible in your current search() path.
## NB most of the methods in the base packages are hidden.
for(fn in methods("predict"))
   try(cat(fn,":\n\t",deparse(args(get(fn))),"\n"), silent = TRUE)
}
\keyword{methods}
