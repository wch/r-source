\name{boxplot.formula}
\alias{boxplot.formula}
\title{Formula Notation for Box Plots}
\description{
  Produce box-and-whisker plot(s) of the given (grouped) values using
  formula notation.
}
\usage{
\method{boxplot}{formula}(formula, data = NULL, \dots, subset)
}
\arguments{
  \item{formula}{a formula, such as \code{y ~ x}.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{\dots}{arguments to the default boxplot method and graphical
    parameters may also be passed as arguments, see \code{\link{par}}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
}
\details{
  This is a method of the generic function \code{\link{boxplot}}.  It
  operates by setting up the data from the formula specification, and
  then calling \code{\link{boxplot.default}}.
}
\references{
  See \code{\link{boxplot.stats}}.
}
\seealso{
  \code{\link{boxplot.default}}
}
\examples{
data(OrchardSprays)
boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col="bisque")
}
\keyword{hplot}
