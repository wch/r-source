\name{rowsum}
\alias{rowsum}
\title{
Give Row Sums of a Matrix, Based on a Grouping Variable
}
\description{
  Compute sums across rows of a matrix for each level of a grouping
  variable.
}
\usage{
rowsum(x, group, reorder = TRUE)
}
\arguments{
  \item{x}{a matrix or vector of numeric data.  Missing values are
    allowed.}
  \item{group}{a vector giving the grouping, with one element per row of
    \code{x}.  Missing values are not allowed.}
  \item{reorder}{if \code{TRUE}, then the result will be in order of
    \code{sort(unique(group))}, if \code{FALSE}, it will be in the order
    that rows were encountered (and may run faster for large matrices).
    The default is to reorder the data, so as to agree with
    \code{tapply} (see example below).}
}
\value{
  a matrix containing the sums.  There will be one row per unique value
  of \code{group}.
}
\author{Terry Therneau}
\seealso{
  \code{\link{tapply}}
}
\examples{
x <- matrix(runif(100), ncol=5)
group <- sample(1:8, 20, T)
xsum <- rowsum(x, group)

## same result another way, slower, and temp may be much larger than x
temp <- model.matrix(~ a - 1, data.frame(a=as.factor(group)))
xsum2<- t(temp) \%*\% x

## same as last one, but really slow
xsum3 <- tapply(x, list(group[row(x)], col(x)), sum)
}
\keyword{manip}
% Converted by Sd2Rd version 0.2-a3.
