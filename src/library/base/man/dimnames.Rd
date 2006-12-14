\name{dimnames}
\title{Dimnames of an Object}
\alias{dimnames}
\alias{dimnames<-}
\alias{dimnames.data.frame}
\alias{dimnames<-.data.frame}
\description{
  Retrieve or set the dimnames of an object.
}
\usage{
dimnames(x, \dots)
\method{dimnames}{data.frame}(x, maybeNULL = FALSE, \dots)
dimnames(x) <- value
}
\arguments{
  \item{x}{an \R object, for example a matrix, array or data frame.}
  \item{\dots}{potentially further arguments for \code{dimnames} methods.}
  \item{maybeNULL}{for the \code{\link{data.frame}} method, a
    \code{\link{logical}} indicating if the \code{dimnames()[[1]]}
    component may be \code{\link{NULL}} when \code{x} is a data frame
    with trivial row names, i.e., rownames equivalent to \code{1:nrow(x)}.}
  \item{value}{a possible value for \code{dimnames(x)}: see
    \dQuote{Value}.}
}
\details{
  The functions \code{dimnames} and \code{dimnames<-} are generic.

  For an \code{\link{array}} (and hence in particular, for a
  \code{\link{matrix}}), they retrieve or set the \code{dimnames}
  attribute (see \link{attributes}) of the object.  A list
  \code{value} can have names, and these will be used to label the
  dimensions of the array where appropriate.

  The replacement method for arrays/matrices coerces vector and factor
  elements of \code{value} to character, but does not dispatch methods for
  \code{as.character}.  It coerces zero-length elements to \code{NULL}.

  Both have methods for data frames.  The dimnames of a data frame are
  its \code{\link{row.names}} and its \code{\link{names}}.  For the
  replacement method each component of \code{value} will be coerced by
  \code{\link{as.character}}.

  For a 1D matrix the \code{\link{names}} are the same thing as the
  (only) component of the \code{dimnames}.
}
\value{
  The dimnames of a matrix or array can be \code{NULL} or a list of the
  same length as \code{dim(x)}.  If a list, its components are either
  \code{NULL} or a character vector with positive length of the
  appropriate dimension of \code{x}.

  For the \code{"data.frame"} method both dimnames are character
  vectors, and the rownames must contain no duplicates nor missing values.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
