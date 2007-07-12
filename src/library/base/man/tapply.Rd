\name{tapply}
\alias{tapply}
\title{Apply a Function Over a ``Ragged'' Array}
\description{
  Apply a function to each cell of a ragged array, that is to each
  (non-empty) group of values given by a unique combination of the
  levels of certain factors.
}
\usage{
tapply(X, INDEX, FUN = NULL, \dots, simplify = TRUE)
}
\arguments{
  \item{X}{an atomic object, typically a vector.}
  \item{INDEX}{list of factors, each of same length as \code{X}.  The
    elements are coerced to factors by \code{\link{as.factor}}.}
  \item{FUN}{the function to be applied.  In the case of functions like
    \code{+}, \code{\%*\%}, etc., the function name must be quoted.  If
    \code{FUN} is \code{NULL}, tapply returns a vector which can be used
    to subscript the multi-way array \code{tapply} normally produces.}
  \item{\dots}{optional arguments to \code{FUN}.}
  \item{simplify}{If \code{FALSE}, \code{tapply} always returns an array
    of mode \code{"list"}.  If \code{TRUE} (the default), then if
    \code{FUN} always returns a scalar, \code{tapply} returns an array
    with the mode of the scalar.}
}
\value{
  When \code{FUN} is present, \code{tapply} calls \code{FUN} for each
  cell that has any data in it.  If \code{FUN} returns a single atomic
  value for each cell (e.g., functions \code{mean} or \code{var}) and
  when \code{simplify} is \code{TRUE}, \code{tapply} returns a multi-way
  \link{array} containing the values.  The array has the same number of
  dimensions as \code{INDEX} has components; the number of levels in a
  dimension is the number of levels (\code{nlevels()}) in the
  corresponding component of \code{INDEX}.

  Note that contrary to S, \code{simplify = TRUE} always returns an
  array, possibly 1-dimensional.

  If \code{FUN} does not return a single atomic value, \code{tapply}
  returns an array of mode \code{\link{list}} whose components are the
  values of the individual calls to \code{FUN}, i.e., the result is a
  list with a \code{\link{dim}} attribute.

  When there is an array answer, its \code{\link{dimnames}} are named by
  the names of \code{INDEX} and are based on the levels of the grouping
  factors (possibly after coercion).

  Note that optional arguments to \code{FUN} supplied by the
  \code{...} argument are not divided into cells.  It is therefore
  inappropriate for \code{FUN} to expect additional arguments with
  the same length as \code{X}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  the convenience functions \code{\link{by}} and
  \code{\link{aggregate}} (using \code{tapply});
  \code{\link{apply}},
  \code{\link{lapply}} with its versions
  \code{\link{sapply}} and \code{\link{mapply}}.
}
\examples{
require(stats)
groups <- as.factor(rbinom(32, n = 5, prob = 0.4))
tapply(groups, groups, length) #- is almost the same as
table(groups)

## contingency table from data.frame : array with named dimnames
tapply(warpbreaks$breaks, warpbreaks[,-1], sum)
tapply(warpbreaks$breaks, warpbreaks[, 3, drop = FALSE], sum)

n <- 17; fac <- factor(rep(1:3, length = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, sum, simplify = FALSE)
tapply(1:n, fac, range)
tapply(1:n, fac, quantile)

## example of ... argument: find quarterly means
tapply(presidents, cycle(presidents), mean, na.rm = TRUE)

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)
}
\keyword{iteration}
\keyword{category}
