% File src/library/base/man/data.class.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{data.class}
\alias{data.class}
\title{Object Classes}
\description{
  Determine the class of an arbitrary \R object.
}
\usage{
data.class(x)
}
\arguments{
  \item{x}{an \R object.}
}
\value{
  character string giving the \emph{class} of \code{x}.

  The class is the (first element) of the \code{\link{class}}
  attribute if this is non-\code{NULL}, or inferred from the object's
  \code{dim} attribute if this is non-\code{NULL}, or \code{mode(x)}.

  Simply speaking, \code{data.class(x)} returns what is typically useful
  for method dispatching.  (Or, what the basic creator functions already
  and maybe eventually all will attach as a class attribute.)
}
\note{
  For compatibility reasons, there is one exception to the rule above:
  When \code{x} is \code{\link{integer}}, the result of
  \code{data.class(x)} is \code{"numeric"} even when \code{x} is classed.
}
\seealso{
  \code{\link{class}}
}
\examples{
x <- LETTERS
data.class(factor(x))                 # has a class attribute
data.class(matrix(x, ncol = 13))      # has a dim attribute
data.class(list(x))                   # the same as mode(x)
data.class(x)                         # the same as mode(x)

stopifnot(data.class(1:2) == "numeric") # compatibility "rule"
}
\keyword{classes}
\keyword{methods}
