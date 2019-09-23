% File src/library/base/man/rowsum.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{rowsum}
\alias{rowsum}
\alias{rowsum.default}
\alias{rowsum.data.frame}
\title{Give Column Sums of a Matrix or Data Frame, Based on a Grouping Variable}
\description{
  Compute column sums across rows of a numeric matrix-like object for
  each level of a grouping variable.  \code{rowsum} is generic, with a
  method for data frames and a default method for vectors and matrices.
}
\usage{
rowsum(x, group, reorder = TRUE, \dots)

\method{rowsum}{data.frame}(x, group, reorder = TRUE, na.rm = FALSE, \dots)

\method{rowsum}{default}(x, group, reorder = TRUE, na.rm = FALSE, \dots)
}
\arguments{
  \item{x}{a matrix, data frame or vector of numeric data.  Missing
    values are allowed.  A numeric vector will be treated as a column vector.}
  \item{group}{a vector or factor giving the grouping, with one element
    per row of \code{x}.  Missing values will be treated as another
    group and a warning will be given.}
  \item{reorder}{if \code{TRUE}, then the result will be in order of
    \code{sort(unique(group))}, if \code{FALSE}, it will be in the order
    that groups were encountered.}
  \item{na.rm}{logical (\code{TRUE} or \code{FALSE}).  Should \code{NA}
    (including \code{NaN}) values be discarded?}
  \item{\dots}{other arguments to be passed to or from methods}
}
\value{
  A matrix or data frame containing the sums.  There will be one row per
  unique value of \code{group}.
}

\details{
  The default is to reorder the rows to agree with \code{tapply} as in
  the example below.  Reordering should not add noticeably to the time
  except when there are very many distinct values of \code{group} and
  \code{x} has few columns.

  The original function was written by Terry Therneau, but this is a
  new implementation using hashing that is much faster for large matrices.

  To sum over all the rows of a matrix (i.e., a single \code{group}) use
  \code{\link{colSums}}, which should be even faster.

  For integer arguments, over/underflow in forming the sum results in
  \code{NA}.
}
\seealso{
  \code{\link{tapply}}, \code{\link{aggregate}}, \code{\link{rowSums}}
}
\examples{
require(stats)

x <- matrix(runif(100), ncol = 5)
group <- sample(1:8, 20, TRUE)
(xsum <- rowsum(x, group))
## Slower versions
tapply(x, list(group[row(x)], col(x)), sum)
t(sapply(split(as.data.frame(x), group), colSums))
aggregate(x, list(group), sum)[-1]
}
\keyword{manip}
