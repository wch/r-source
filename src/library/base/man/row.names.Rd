\name{row.names}
\title{Get and Set Row Names for Data Frames}
\alias{row.names}
\alias{row.names.data.frame}
\alias{row.names.default}
\alias{row.names<-}
\alias{row.names<-.data.frame}
\alias{row.names<-.default}
\description{
  All data frames have a row names attribute, a character vector of
  length the number of rows with no duplicates nor missing values.

  For convenience, these are generic functions for which users can write
  other methods, and there are default methods for arrays.  The
  description here is for the \code{data.frame} method.
}
\usage{
row.names(x)
row.names(x) <- value
}
\arguments{
  \item{x}{object of class \code{"data.frame"}, or any other class for
    which a method has been defined.}
  \item{value}{an object to be coerced to character unless an integer
    vector.  It should have (after coercion) the same length as the
    number of rows of \code{x} with no duplicated nor missing values.
    \code{NULL} is also allowed: see Details.}
}
\details{
  A data frame has (by definition) a vector of \emph{row names} which
  has length the number of rows in the data frame, and contains neither
  missing nor duplicated values.  Where a row names sequence has been
  added by the software to meet this requirement, they are regarded as
  \sQuote{automatic}.

  Prior to \R 2.4.0, row names were character.  As from \R 2.4.0, they
  are allowed to be integer or character, but for backwards
  compatibility \code{row.names} will always return a character vector.
  (Use \code{attr(x, "row.names")} if you need an integer value.)

  Using \code{NULL} for the value resets the row names to
  \code{seq_len(nrow(x))}, regarded as \sQuote{automatic}.
}
\value{
  \code{row.names} returns a character vector.

  \code{row.names<-} returns a data frame with the row names changed.
}
\note{
  \code{row.names} is similar to \code{\link{rownames}} for arrays, and
  it has a method that calls \code{rownames} for an array argument.

  Row names of the form \code{1:n} for \code{n > 2} are stored
  internally in a compact form, which might be seen from C code or by
  deparsing but never via \code{row.names} or
  \code{\link{attr}(x, "row.names")}.  Additionally, some names of this
  sort are marked as \sQuote{automatic} and handled differently by
  \code{\link{as.matrix}} and \code{\link{data.matrix}} (and potentially
  other functions).  (All zero-row data frames are regarded as having
  automatic row.names.)
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}}, \code{\link{rownames}}, \code{\link{names}}.

  \code{\link{.row_names_info}} for the internal representations.
}
\keyword{classes}
\keyword{methods}
