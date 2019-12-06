% File src/library/utils/man/head.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{head}
\title{Return the First or Last Parts of an Object}
\alias{head}
\alias{head.data.frame}
\alias{head.default}
\alias{head.function}
\alias{head.ftable}
\alias{head.matrix}
\alias{tail}
\alias{tail.data.frame}
\alias{tail.default}
\alias{tail.function}
\alias{tail.ftable}
\alias{tail.table}
\alias{tail.matrix}
\alias{tail.array}
\description{
  Returns the first or last parts of a vector, matrix, table, data frame
  or function.  Since \code{head()} and \code{tail()} are generic
  functions, they may also have been extended to other classes.
}
\usage{
head(x, \dots)
\method{head}{default}(x, n = 6L, \dots)
% head.matrix(x, n = 6L, \dots) # <-- gives NOTE, even though exported
\method{head}{matrix}(x, n = 6L, \dots) # is exported as head.matrix()
## NB: The methods for 'data.frame' and 'array'  are identical to the 'matrix' one

\method{head}{ftable}(x, n = 6L, \dots)
\method{head}{function}(x, n = 6L, \dots)


tail(x, \dots)
\method{tail}{default}(x, n = 6L, addrownums = FALSE, \dots)
\method{tail}{matrix}(x, n = 6L, addrownums = TRUE, \dots) # exported as tail.matrix()
## NB: The methods for 'data.frame', 'array', and 'table'
##     are identical to the  'matrix'  one

\method{tail}{ftable}(x, n = 6L, addrownums = FALSE, \dots)
\method{tail}{function}(x, n = 6L, \dots)
}
\arguments{
  \item{x}{an object}
  \item{n}{an integer vector of length up to \code{dim(x)} (or 1,
    for non-dimensioned objects). Values specify the indices to be
    selected in the corresponding dimension (or along the length) of the
    object. A positive value of \code{n[i]} includes the first/last
    \code{n[i]} indices in that dimension, while a negative value
    excludes the last/first \code{abs(n[i])}, including all remaining
    indices. \code{NA} or non-specified values (when \code{length(n) <
      length(dim(x))}) select all indices in that dimension.}
  \item{addrownums}{if there are no row names, create them from the row
    numbers.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\details{
  For vector/array based objects, \code{head()} (\code{tail()}) returns
  a subset of the same dimensionality as \code{x}, usually of
  the same class. For historical reasons, by default they select the
  first (last) 6 indices in the first dimension ("rows") or along the
  length of a non-dimensioned vector, and the full extent (all indices)
  in any remaining dimensions. \code{head.matrix()} and
  \code{tail.matrix()} are exported.

  The default and array(/matrix) methods for \code{head()} and
  \code{tail()} are quite general. They will work as is for any class
  which has a \code{dim()} method, a \code{length()} method (only
  required if \code{dim()} returns \code{NULL}), and a \code{[} method
  (that accepts the \code{drop} argument and can subset in all
  dimensions in the dimensioned case).

  For functions, the lines of the deparsed function are returned as
  character strings.

  If an array(/matrix) has no row names, then \code{tail()} will add row
  names of the form \code{"[n,]"} to the result, so that it looks
  similar to the last lines of \code{x} when printed.  Setting
  \code{addrownums = FALSE} suppresses this behaviour.

  As \code{\link{data.frame}} subsetting (\sQuote{indexing}) keeps
  \code{\link{attributes}}, so do the \code{head()} and \code{tail()}
  methods for data frames.
}
\value{
  An object (usually) like \code{x} but generally smaller.  Hence, for
  \code{\link{array}}s, the result corresponds to \code{x[.., drop=FALSE]}.
  For \code{\link{ftable}} objects \code{x}, a transformed \code{format(x)}.
}
\author{
  Patrick Burns, improved and corrected by R-Core. Negative argument
  added by Vincent Goulet.  Multi-dimension support added by Gabriel Becker.
}

\examples{
head(letters)
head(letters, n = -6L)

head(freeny.x, n = 10L)
head(freeny.y)

head(iris3)
head(iris3, c(6L, 2L))
head(iris3, c(6L, -1L, 2L))

tail(letters)
tail(letters, n = -6L)

tail(freeny.x)
## the bottom-right "corner" :
tail(freeny.x, n = c(4, 2))
tail(freeny.y)

tail(iris3)
tail(iris3, c(6L, 2L))
tail(iris3, c(6L, -1L, 2L))

## data frame w/ a (non-standard) attribute:
treeS <- structure(trees, foo = "bar")
(n <- nrow(treeS))
stopifnot(exprs = { # attribute is kept
    identical(htS <- head(treeS), treeS[1:6, ])
    identical(attr(htS, "foo") , "bar")
    identical(tlS <- tail(treeS), treeS[(n-5):n, ])
    ## BUT if I use "useAttrib(.)", this is *not* ok, when n is of length 2:
    ## --- because [i,j]-indexing of data frames *also* drops "other" attributes ..
    identical(tail(treeS, 3:2), treeS[(n-2):n, 2:3] )
})

tail(library) # last lines of function

head(stats::ftable(Titanic))

## 1d-array (with named dim) :
a1 <- array(1:7, 7); names(dim(a1)) <- "O2"
stopifnot(exprs = {
  identical( tail(a1, 10), a1)
  identical( head(a1, 10), a1)
  identical( head(a1, 1), a1 [1 , drop=FALSE] ) # was a1[1] in R <= 3.6.x
  identical( tail(a1, 2), a1[6:7])
  identical( tail(a1, 1), a1 [7 , drop=FALSE] ) # was a1[7] in R <= 3.6.x
})
}
\keyword{manip}
