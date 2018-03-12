% File src/library/base/man/row.names.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{row.names}
\title{Get and Set Row Names for Data Frames}
\alias{row.names}
\alias{row.names.data.frame}
\alias{row.names.default}
\alias{row.names<-}
\alias{row.names<-.data.frame}
\alias{row.names<-.default}
\alias{.rowNamesDF<-}
\description{
  All data frames have a row names attribute, a character vector of
  length the number of rows with no duplicates nor missing values.

  For convenience, these are generic functions for which users can write
  other methods, and there are default methods for arrays.  The
  description here is for the \code{data.frame} method.

  \code{`.rowNamesDF<-`} is a (non-generic replacement) function to set
  row names for data frames, with extra argument \code{make.names}.
  This function only exists as workaround as we cannot easily change the
  \code{row.names<-} generic without breaking legacy code in existing packages.
  %% would like generic (and default)   row.names(x, ...) <- value
}
\usage{
row.names(x)
row.names(x) <- value
.rowNamesDF(x, make.names=FALSE) <- value
}
\arguments{
  \item{x}{object of class \code{"data.frame"}, or any other class for
    which a method has been defined.}
  \item{make.names}{\code{\link{logical}}, i.e., one of \code{FALSE, NA, TRUE},
    indicating what should happen if the specified row names, i.e.,
    \code{value}, are invalid, e.g., duplicated or \code{NA}.  The default
    (is back compatible), \code{FALSE}, will signal an error, where
    \code{NA} will \dQuote{automatic} row names and \code{TRUE} will call
    \code{\link{make.names}(value, unique=TRUE)} for constructing valid
    names.}
  \item{value}{an object to be coerced to character unless an integer
    vector.  It should have (after coercion) the same length as the
    number of rows of \code{x} with no duplicated nor missing values.
    \code{NULL} is also allowed: see \sQuote{Details}.}
}
\details{
  A data frame has (by definition) a vector of \emph{row names} which
  has length the number of rows in the data frame, and contains neither
  missing nor duplicated values.  Where a row names sequence has been
  added by the software to meet this requirement, they are regarded as
  \sQuote{automatic}.

  Row names are currently allowed to be integer or character, but
  for backwards compatibility (with \R <= 2.4.0) \code{row.names} will
  always return a character vector.  (Use \code{attr(x, "row.names")} if
  you need to retrieve an integer-valued set of row names.)

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
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{data.frame}}, \code{\link{rownames}}, \code{\link{names}}.

  \code{.row_names_info} for the internal representations.
}
\keyword{classes}
\keyword{methods}
