% File src/library/methods/man/canCoerce.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{canCoerce}
\alias{canCoerce}
\title{Can an Object be Coerced to a Certain S4 Class?}
\description{
  Test if an object can be coerced to a given S4 class.
  Maybe useful inside \code{if()} to ensure that calling
  \code{as(object, Class)} will find a method.
}
\usage{
canCoerce(object, Class)
}
\arguments{
  \item{object}{any \R object, typically of a formal S4 class.}
  \item{Class}{an S4 class (see \code{\link{isClass}}).}
}
\value{
  a scalar logical, \code{TRUE} if there is a \code{coerce} method
  (as defined by e.g.\sspace{}\code{\link{setAs}}) for the signature
  \code{(from = class(object), to = Class)}.
}
\seealso{\code{\link{as}}, \code{\link{setAs}},
  \code{\link{selectMethod}}, \code{\link{setClass}},
}
\examples{
m <- matrix(pi, 2,3)
canCoerce(m, "numeric") # TRUE
canCoerce(m, "array")   # TRUE
}
%% FIXME: give an interesting one returning FALSE
\keyword{classes}
\keyword{methods}
