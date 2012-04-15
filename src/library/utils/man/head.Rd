% File src/library/utils/man/head.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{head}
\alias{head}
\alias{head.default}
\alias{head.data.frame}
\alias{head.function}
\alias{head.ftable}
\alias{head.table}
\alias{head.matrix}
\alias{tail}
\alias{tail.default}
\alias{tail.data.frame}
\alias{tail.function}
\alias{tail.ftable}
\alias{tail.table}
\alias{tail.matrix}
\title{
  Return the First or Last Part of an Object
}
\description{
  Returns the first or last parts of a vector, matrix, table, data frame
  or function.  Since \code{head()} and \code{tail()} are generic
  functions, they may also have been extended to other classes.
}
\usage{
head(x, \dots)
\method{head}{default}(x, n = 6L, \dots)
\method{head}{data.frame}(x, n = 6L, \dots)
\method{head}{matrix}(x, n = 6L, \dots)
\method{head}{ftable}(x, n = 6L, \dots)
\method{head}{table}(x, n = 6L, \dots)
\method{head}{function}(x, n = 6L, \dots)

tail(x, \dots)
\method{tail}{default}(x, n = 6L, \dots)
\method{tail}{data.frame}(x, n = 6L, \dots)
\method{tail}{matrix}(x, n = 6L, addrownums = TRUE, \dots)
\method{tail}{ftable}(x, n = 6L, addrownums = FALSE, \dots)
\method{tail}{table}(x, n = 6L, addrownums = TRUE, \dots)
\method{tail}{function}(x, n = 6L, \dots)
}
\arguments{
  \item{x}{an object}
  \item{n}{a single integer. If positive, size for the resulting
    object: number of elements for a vector (including lists), rows for
    a matrix or data frame or lines for a function. If negative, all but
    the \code{n} last/first number of elements of \code{x}.}
  \item{addrownums}{if there are no row names, create them from the row
    numbers.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  For matrices, 2-dim tables and data frames, \code{head()} (\code{tail()}) returns
  the first (last) \code{n} rows when \code{n > 0} or all but the
  last (first) \code{n} rows when \code{n < 0}.  \code{head.matrix()} and
  \code{tail.matrix()} are exported.  For functions, the
  lines of the deparsed function are returned as character strings.

  If a matrix has no row names, then \code{tail()} will add row names of
  the form \code{"[n,]"} to the result, so that it looks similar to the
  last lines of \code{x} when printed.  Setting \code{addrownums =
    FALSE} suppresses this behaviour.
}
\value{
  An object (usually) like \code{x} but generally smaller.  For
  \code{\link{ftable}} objects \code{x}, a transformed \code{format(x)}.
}
\author{
  Patrick Burns, improved and corrected by R-Core. Negative argument
  added by Vincent Goulet.
}

\examples{
head(letters)
head(letters, n = -6L)

head(freeny.x, n = 10L)
head(freeny.y)

tail(letters)
tail(letters, n = -6L)

tail(freeny.x)
tail(freeny.y)

tail(library)

head(stats::ftable(Titanic))
}
\keyword{ manip }
