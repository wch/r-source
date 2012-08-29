% File src/library/base/man/dimnames.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

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
dimnames(x)
dimnames(x) <- value
}
\arguments{
  \item{x}{an \R object, for example a matrix, array or data frame.}
  \item{value}{a possible value for \code{dimnames(x)}: see the
    \sQuote{Value} section.}
}
\details{
  The functions \code{dimnames} and \code{dimnames<-} are generic.

  For an \code{\link{array}} (and hence in particular, for a
  \code{\link{matrix}}), they retrieve or set the \code{dimnames}
  attribute (see \link{attributes}) of the object.  A list
  \code{value} can have names, and these will be used to label the
  dimensions of the array where appropriate.

  The replacement method for arrays/matrices coerces vector and factor
  elements of \code{value} to character, but does not dispatch methods
  for \code{as.character}.  It coerces zero-length elements to
  \code{NULL}, and a zero-length list to \code{NULL}.  If \code{value}
  is a list shorter than the number of dimensions, it is extended with
  \code{NULL}s to the needed length.
  
  Both have methods for data frames.  The dimnames of a data frame are
  its \code{\link{row.names}} and its \code{\link{names}}.  For the
  replacement method each component of \code{value} will be coerced by
  \code{\link{as.character}}.

  For a 1D matrix the \code{\link{names}} are the same thing as the
  (only) component of the \code{dimnames}.
  
  Both are \link{primitive} functions.  
}
\value{
  The dimnames of a matrix or array can be \code{NULL} (which is not
  stored) or a list of the same length as \code{dim(x)}.  If a list, its
  components are either \code{NULL} or a character vector with positive
  length of the appropriate dimension of \code{x}.  The list can have
  names.  It is possible that all components are \code{NULL}: such
  dimnames may get converted to \code{NULL}.

  For the \code{"data.frame"} method both dimnames are character
  vectors, and the rownames must contain no duplicates nor missing values.
}
\note{
  Setting components of the dimnames, e.g.
  \code{dimnames(A)[[1]] <- value} is a common paradigm, but note that
  it will not work if the value assigned is \code{NULL}.  Use
  \code{\link{rownames}} instead, or (as it does) manipulate the whole
  dimnames list.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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
