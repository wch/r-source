\name{attr}
\alias{attr}
\alias{attr<-}
\title{Object Attributes}
\description{
  Get or set specific attributes of an object.
}
\usage{
attr(x, which)
attr(x, which) <- value
}
\arguments{
  \item{x}{an object whose attributes are to be accessed.}
  \item{which}{a character string specifying which attribute is to be
    accessed.}
  \item{value}{an object, the new value of the attribute.}
}
\value{
  This function provides access to a single object attribute.  The
  simple form above returns the value of the named attribute.  The
  assignment form causes the named attribute to take the value on the
  right of the assignment symbol.

  The first form first looks for an exact match to \code{code} amongst the
  attributed of \code{x}, then a partial match.  If no exact match is
  found and more than one partial match is found, the result is \code{NULL}.

  Note that some attributes (namely \code{class}, \code{comment},
  \code{dim}, \code{dimnames}, \code{names}
  and \code{tsp}) are treated specially and have restrictions on the
  values which can be set.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{attributes}}
}
\examples{
# create a 2 by 5 matrix
x <- 1:10
attr(x,"dim") <- c(2, 5)
}
\keyword{attribute}
