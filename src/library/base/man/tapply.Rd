\name{tapply}
\title{Apply a Function Over a ``Ragged'' Array}
\usage{
tapply(x, INDEX, FUN = NULL, simplify = TRUE, \dots)
}
\alias{tapply}
\arguments{
  \item{x}{an atomic object, typically a vector.}
  \item{INDEX}{list of factors, each of same length as \code{x}.}
  \item{FUN}{the function to be applied.  In the case of functions like
    \code{+}, \code{\%*\%}, etc., the function name must be quoted.  If
    \code{FUN} is \code{NULL}, tapply returns a vector which can be used
    to subscript the multi-way array \code{tapply} normally produces.}
  \item{simplify}{If \code{FALSE}, \code{tapply} always returns an array
    of mode \code{"list"}.  If \code{TRUE} (the default), then if
    \code{FUN} always returns a scalar, \code{tapply} returns an array
    with the mode of the scalar, and if the array would be one
    dimensional the dimension is removed, to make it a vector.}
  \item{\dots}{optional arguments to \code{FUN}.}
}
\value{
  When \code{FUN} is present, \code{tapply} calls \code{FUN} for each
  cell that has any data in it.  If \code{FUN} returns a single atomic
  value for each cell (e.g., functions \code{mean} or \code{var}), then
  \code{tapply} returns a multi-way array containing the values.  The
  array has the same number of dimensions as \code{INDEX} has
  components; the number of levels in a dimension is the number of
  levels (\code{nlevels(.)}) in the corresponding component of
  \code{INDEX}.  This is a vector if \code{INDEX} has only one
  component.

  If \code{FUN} does not return a single atomic value, \code{tapply}
  returns an array of mode \code{"\link{list}"} whose components are the
  values of the individual calls to \code{FUN}, i.e., the result is a
  list with a \code{\link{dim}} attribute.
}
\seealso{
  \code{\link{apply}}, \code{\link{lapply}} with its version
  \code{sapply}.
}
\examples{
groups <- as.factor(rbinom(32, n = 5, p = .4))
tapply(groups, groups, length) #- is almost the same as
table(groups)

n <- 17; fac <- factor(rep(1:3, len = n), levels = 1:5)
table(fac)
tapply(1:n, fac, sum)
tapply(1:n, fac, sum, simplify = FALSE) #- does not yet print okay
tapply(1:n, fac, range)			#- .. nor does this
tapply(1:n, fac, quantile)		#- ... or this

ind <- list(c(1, 2, 2), c("A", "A", "B"))
table(ind)
tapply(1:3, ind) #-> the split vector
tapply(1:3, ind, sum)
}
\keyword{iteration}
\keyword{category}
