\name{drop}
\alias{drop}
\title{Drop Redundant Extent Information}
\description{
  Delete the dimensions of an array which have only one level.
}
\usage{
drop(x)
}
\arguments{
  \item{x}{an array (including a matrix).}
}
\value{
  If \code{x} is an object with a \code{dim} attribute (e.g., a matrix
  or \code{\link{array}}), then \code{drop} returns an object like
  \code{x}, but with any extents of length one removed.  Any
  accompanying \code{dimnames} attribute is adjusted and returned with
  \code{x}.

  Array subsetting (\code{\link{[}}) performs this reduction unless used
  with \code{drop = FALSE}, but sometimes it is useful to invoke
  \code{drop} directly.
}
\seealso{
  \code{\link{drop1}} which is used for dropping terms in models.
}
\examples{
dim(drop(array(1:12, dim=c(1,3,1,1,2,1,2))))# = 3 2 2
drop(1:3 \%*\% 2:4)# scalar product
}
\keyword{array}
