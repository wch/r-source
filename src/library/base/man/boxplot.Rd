\name{boxplot}
\alias{boxplot}
\title{Box Plots}
\usage{
boxplot(x, \dots)
}
\arguments{
    \item{x}{an R object.}
    \item{\dots}{methods may have additional arguments.}
}
\description{
    Produce box-and-whisker plot(s) of the given (grouped) values.
}
\details{
    This is a generic function.  It currently has a default method
    (\code{\link{boxplot.default}}) and a formula interface
    (\code{\link{boxplot.formula}}).
}
\seealso{
    \code{\link{boxplot.default}},
    \code{\link{boxplot.formula}}.
}
\examples{
## boxplot on a formula:
data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")

## boxplot on a matrix:
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
             T5 = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
boxplot(data.frame(mat), main = "boxplot(data.frame(mat), main = ...)")
}
\keyword{hplot}
