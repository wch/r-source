\name{rowsum}
\alias{rowsum}
\alias{rowsum.default}
\alias{rowsum.data.frame}
\title{
Give row sums of a matrix or data frame, based on a grouping variable
}
\description{
  Compute sums across rows of a matrix-like object for each level of a grouping
  variable. \code{rowsum} is generic, with methods for matrices and data
  frames.
}
\usage{
rowsum(x, group, reorder = TRUE, \dots)
}
\arguments{
  \item{x}{a matrix, data frame or vector of numeric data.  Missing
  values are    allowed.}
  \item{group}{a vector giving the grouping, with one element per row of
    \code{x}.  Missing values will be treated as another group and a
    warning will be given}
  \item{reorder}{if \code{TRUE}, then the result will be in order of
    \code{sort(unique(group))}, if \code{FALSE}, it will be in the order
    that rows were encountered. }
  \item{\dots}{other arguments for future methods}
}
\value{
  a matrix or data frame containing the sums.  There will be one row per
  unique value  of \code{group}.
}

\details{
  The default is to reorder the rows to agree with \code{tapply} as in
  the example below. Reordering should not add noticeably to the time
  except when there are very many distinct values of \code{group} and
  \code{x} has few columns.

  The original function was written by Terry Therneau, but this is a
  new implementation using hashing that is much faster for large matrices.

  To add all the rows of a matrix (ie, a single \code{group}) use
  \code{\link{rowSums}}, which should be even faster.
}

\seealso{
  \code{\link{tapply}}, \code{\link{aggregate}}, \code{\link{rowSums}}
}
\examples{
x <- matrix(runif(100), ncol=5)
group <- sample(1:8, 20, TRUE)
xsum <- rowsum(x, group)
## Slower versions
xsum2 <- tapply(x, list(group[row(x)], col(x)), sum)
xsum3<- aggregate(x,list(group),sum)

}
\keyword{manip}

