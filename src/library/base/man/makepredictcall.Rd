\name{makepredictcall}
\alias{makepredictcall}
\alias{makepredictcall.bs}
\alias{makepredictcall.ns}
\alias{makepredictcall.poly}
\title{Utility Function for Safe Prediction}
\description{
  A utility to help \code{\link{model.frame.default}} create the right
  matrices when predicting from models with terms like \code{poly} or
  \code{ns}. 
}
\usage{
makepredictcall(var, call)
}
\arguments{
  \item{var}{A variable.}
  \item{call}{The term in the formula, as a call.}
}
\details{
  This is a generic function with methods for \code{poly}, \code{bs} and
  \code{ns}.  If \code{model.frame.default} encounters such a term when
  creating a model frame, it modifies the  \code{predvars} attribute of
  the terms supplied to replace the term with one that will work for
  predicting new data.  For example \code{makepredictcall.ns} adds
  arguments for the knots and intercept.

  To make use of this, have your model-fitting function return the
  \code{terms} attribute of the model frame.

  To extend this, make sure the term creastes variables with a class,
  and write a suitable method for that class.
}
\value{
  A replacement for \code{call} for the \code{predvars} attribute of
  the terms.
}
\seealso{
  \code{\link{model.frame}}, \code{\link{poly}},
  \code{\link[splines]{predict.bs}}, \code{\link[splines]{predict.ns}}.
}
\keyword{models}
