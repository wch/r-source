\name{boxplot.stats}
\title{Box Plot Statistics}
\usage{
boxplot.stats(x, coef = 1.5)
}
\alias{boxplot.stats}
\arguments{
\item{x}{a numeric vector for which the boxplot will
be constructed (\code{NA}s are allowed).}
\item{coef}{this determines how far the plot ``whiskers''
extend out from the box.
If \code{coef} is positive,
the whiskers extend to the most extreme data point which
is no more than \code{coef} times the interquartile coef
from the box.
A value of zero causes the whiskers to extend to the data extremes.}
}
\value{
This function is typically is called by \code{boxplot} to
gather the statistics necessary for producing box plots,
but may be invoked separately.  The value it returns is
a list with named components as follows:
\item{stats}{a vector containing the extreme of the
lower whisker, the lower ``hinge'', the median, the upper ``hinge''
and the extreme of the upper whisker.}
\item{n}{the number of observations in the sample.}
\item{conf}{the lower and upper extremes of the ``notch''.}
\item{out}{the values of any data points which lie beyond the
extremes of the whiskers.}
}
\seealso{
\code{\link{boxplot}}, \code{\link{bxp}}.
}
\examples{
boxplot.stats(1:100)
boxplot.stats(1:100, coef = 3)
boxplot.stats(1:100, coef = 0)
}
\keyword{dplot}
