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
  \item{value}{a vector with the same length as the number of rows of
    \code{x}, to be coerced to character.  Duplicated or missing values
    are not allowed.}
}
\value{
  \code{row.names} returns a character vector.

  \code{row.names<-} returns a data frame with the row names changed.
}
\note{
  \code{row.names} is similar to \code{\link{rownames}} for arrays, and
  it has a method that calls \code{rownames} for an array argument.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}}, \code{\link{rownames}}.
}
\keyword{classes}
\keyword{methods}
