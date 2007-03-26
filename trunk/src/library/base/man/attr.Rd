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
  \item{which}{a non-empty character string specifying which attribute
    is to be accessed.}
  \item{value}{an object, the new value of the attribute, or \code{NULL}
    to remove the attribute.}
}
\details{
  These functions provide access to a single attribute of an object.
  The replacement form causes the named attribute to take the value
  specified (or create a new attribute with the value given).

  The extraction function first looks for an exact match to \code{which}
  amongst the attributes of \code{x}, then a unique partial match.  The
  replacement function only uses exact matches.

  Note that some attributes (namely \code{\link{class}},
  \code{\link{comment}}, \code{\link{dim}}, \code{\link{dimnames}},
  \code{\link{names}}, (from \R 2.4.0) \code{\link{row.names}} and
  \code{\link{tsp}}) are treated specially and have restrictions on
  the values which can be set.

  The extractor allows (and does not match) empty and missing values of
  \code{which}: the replacement function does not.
}
\value{
  For the extractor, the value of the attribute matched, or \code{NULL}
  if no exact match is found and no or more than one partial match is found.
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
