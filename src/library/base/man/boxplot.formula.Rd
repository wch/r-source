\name{boxplot.formula}
\alias{boxplot.formula}
\title{Formula Notation for Boxplots}
\usage{
boxplot.formula(formula, data = NULL, subset, na.action, \dots)
}
\arguments{
    \item{formula}{a formula, such as \code{y ~ x}.}
    \item{data}{a data.frame (or list) from which the variables in
	\code{formula} should be taken.}
    \item{subset}{an optional vector specifying a subset of observations
	to be used in the fitting process.}
    \item{na.action}{a function which indicates what should happen when
	the data contain \code{NA}s.}
    \item{\dots}{further arguments to the default boxplot method and
	graphical parameters may also be passed as arguments, see
	\code{\link{boxplot.default}} and \code{\link{par}}.}
}
\description{
    Produces side by side boxplots from a number of vectors, using
    formula notation.
}
\details{
    This is a method of the generic function \code{\link{boxplot}}.  It
    operates by setting up the data from the formula specification, and
    then calling \code{\link{boxplot.default}}.
}
\seealso{
    \code{\link{boxplot}},
    \code{\link{boxplot.default}}.
}
\examples{
data(OrchardSprays)
boxplot(decrease ~ treatment, data = OrchardSprays, log = "y", col = "bisque")
}
\keyword{hplot}
