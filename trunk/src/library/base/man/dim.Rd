\name{dim}
\alias{dim}
\alias{dim.data.frame}
\alias{dim<-}
\title{Dimensions of an Object}
\usage{
dim(x)
dim(x) <- value
}
\description{
  Retrieve or set the dimension of an object.
}
\arguments{
  \item{x}{an \R object, for example a matrix, array or data frame.}
  \item{value}{For the default method, either \code{NULL} or
    a numeric vector which coerced to integer (by truncation).}
}
\details{
  The functions \code{dim} and \code{dim<-} are generic.

  \code{dim} has a method for \code{\link{data.frame}}s, which returns
  the length of the \code{row.names} attribute of \code{x} and the
  length of \code{x} (the numbers of \dQuote{rows} and \dQuote{columns}).
}
\value{
  For an array (and hence in particular, for a matrix) \code{dim} retrieves
  the \code{dim} attribute of the object.  It is \code{NULL} or a vector
  of mode \code{\link{integer}}.

  The replacemnt method changes the \code{"dim"} attribute (provided the
  new value is compatible) and removes any \code{"dimnames"} \emph{and}
  \code{"names"} attributes.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{ncol}}, \code{\link{nrow}} and \code{\link{dimnames}}.
}
\examples{
x <- 1:12 ; dim(x) <- c(3,4)
x

# simple versions of nrow and ncol could be defined as follows
nrow0 <- function(x) dim(x)[1]
ncol0 <- function(x) dim(x)[2]
}
\keyword{array}
