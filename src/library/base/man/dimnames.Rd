\name{dimnames}
\title{Dimnames of an Object}
\alias{dimnames}
\alias{dimnames<-}
\alias{dimnames.data.frame}
\alias{dimnames<-.data.frame}
\usage{
dimnames(x)
dimnames(x) <- nlist
}
\description{
  Retrieve or set the dimnames of an object.
}
\arguments{
  \item{x}{an \R object, for example a matrix, array or data frame.}
  \item{nlist}{a list of the length \code{dim(x)} whose components
    are either null or character vectors the length of the appropriate
    dimension of \code{x}.}
}
\details{
  The functions \code{dimnames} and \code{dimnames<-} are generic.

  For an \code{\link{array}} (and hence in particular, for a
  \code{\link{matrix}}), they retrieve or
  set the \code{dimnames} attribute (see \link{attributes}) of the
  object. The list \code{nlist} can have names, and these will be
  used to label the dimensions of the array where appropriate.

  Both have methods for data frames.  The dimnames of a data frame are
  its \code{row.names} attribute and its \code{\link{names}}.
}
\seealso{
  \code{\link{rownames}}, \code{\link{colnames}};
  \code{\link{array}}, \code{\link{matrix}}, \code{\link{data.frame}}.
}
\examples{
## simple versions of rownames and colnames
## could be defined as follows
rownames0 <- function(x) dimnames(x)[[1]]
colnames0 <- function(x) dimnames(x)[[2]]
}
\keyword{array}
\keyword{manip}
