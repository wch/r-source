\name{pairs.formula}
\alias{pairs.formula}
\title{Formula Notation for Scatterplot Matrices}
\description{
  Produce a matrix of scatterplots using formula notation.
}
\usage{
\method{pairs}{formula}(formula, data = NULL, \dots, subset)
}
\arguments{
  \item{formula}{a formula, such as \code{y ~ x}.}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{\dots}{arguments to the default pairs method and graphical
    parameters may also be passed as arguments, see \code{\link{par}}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
}
\details{
  This is a method of the generic function \code{\link{pairs}}.  It
  operates by setting up the data from the formula specification, and
  then calling \code{\link{pairs.default}}.
}
\seealso{
  \code{\link{pairs.default}}
}
\examples{
data(swiss)
pairs(~ Fertility + Education + Catholic, data = swiss,
      subset = Education < 20, main = "Swiss data, Education < 20")
}
\keyword{hplot}
