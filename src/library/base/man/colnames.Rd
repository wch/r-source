% File src/library/base/man/colnames.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{row+colnames}
\title{Row and Column Names}
\alias{rownames}
\alias{rownames<-}
\alias{colnames}
\alias{colnames<-}
\usage{
rownames(x, do.NULL = TRUE, prefix = "row")
rownames(x) <- value

colnames(x, do.NULL = TRUE, prefix = "col")
colnames(x) <- value
}
\description{
  Retrieve or set the row or column names of a matrix-like object.
}
\arguments{
  \item{x}{a matrix-like \R object, with at least two dimensions for
    \code{colnames}.}
  \item{do.NULL}{logical. If \code{FALSE} and names are \code{NULL}, names are created.}
  \item{prefix}{for created names.}
  \item{value}{a valid value for that component of
    \code{\link{dimnames}(x)}.  For a matrix or array this is either
    \code{NULL} or a character vector of non-zero length equal to the
    appropriate dimension.}
}
\details{
  The extractor functions try to do something sensible for any
  matrix-like object \code{x}.  If the object has \code{\link{dimnames}}
  the first component is used as the row names, and the second component
  (if any) is used for the column names.  For a data frame, \code{rownames}
  and \code{colnames} eventually call \code{\link{row.names}} and
  \code{\link{names}} respectively, but the latter are preferred.

  If \code{do.NULL} is \code{FALSE}, a character vector (of length
  \code{\link{NROW}(x)} or \code{\link{NCOL}(x)}) is returned in any
  case, prepending \code{prefix} to simple numbers, if there are no
  dimnames or the corresponding component of the dimnames is \code{NULL}.

  The replacement methods for arrays/matrices coerce vector and factor
  values of \code{value} to character, but do not dispatch methods for
  \code{as.character}.

  For a data frame, \code{value} for \code{rownames} should be a
  character vector of non-duplicated and non-missing names (this is
  enforced), and for \code{colnames} a character vector of (preferably)
  unique syntactically-valid names.  In both cases, \code{value} will be
  coerced by \code{\link{as.character}}, and setting \code{colnames}
  will convert the row names to character.
}
\note{
  If the replacement versions are called on a matrix without any
  existing dimnames, they will add suitable dimnames.  But
  constructions such as
\preformatted{    rownames(x)[3] <- "c"}
  may not work unless \code{x} already has dimnames, since this will
  create a length-3 \code{value} from the \code{NULL} value of
  \code{rownames(x)}.
}
\seealso{
  \code{\link{dimnames}},
  \code{\link{case.names}},
  \code{\link{variable.names}}.
}
\examples{
m0 <- matrix(NA, 4, 0)
rownames(m0)

m2 <- cbind(1, 1:4)
colnames(m2, do.NULL = FALSE)
colnames(m2) <- c("x","Y")
rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")
m2
}
\keyword{array}
\keyword{manip}
