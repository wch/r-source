\name{split}
\title{Divide into Groups}
\usage{
split(x, f)
split.default(x, f)
split.data.frame(x, f)
}
\alias{split}
\alias{split.default}
\alias{split.data.frame}
\arguments{
\item{x}{vector containing the values to be divided into groups.}
\item{f}{a ``factor'' such that \code{as.factor(f)} defines the grouping.}
}
\description{
  \code{split} divides the data in the vector \code{x} into the groups
  defined by the factor \code{f}.
}
\details{ 
  \code{f} is recycled as necessary and
  if the length of \code{x} is not a multiple of the length of \code{f}
  a warning is printed.
}
\value{
  The value returned is a list of vectors containing the values for the
  groups.  The components of the list are named by the factor levels of
  \code{f}. If \code{f} is longer than \code{x} some of these will be of
  zero length.
}
\seealso{\code{\link{cut}}}
\examples{
n <- 10; nn <- 100
g <- factor(round(n * runif(n * nn)))
x <- rnorm(n * nn) + sqrt(codes(g))
xg <- split(x, g)
boxplot(xg, col = "lavender", notch = TRUE, varwidth = TRUE)
sapply(xg, length)
sapply(xg, mean)

## Split a matrix into a list by columns
ma <- cbind(x = 1:10, y = (-4:5)^2)
split(ma, col(ma))

split(1:10, 1:2)
}
\keyword{category}
