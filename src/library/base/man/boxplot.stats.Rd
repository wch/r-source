\name{boxplot.stats}
\title{Box Plot Statistics}
\usage{
boxplot.stats(x, coef = 1.5)
}
\alias{boxplot.stats}
\arguments{
\item{x}{a numeric vector for which the boxplot will
  be constructed (\code{\link{NA}}s and \code{\link{NaN}}s are allowed
  and omitted).}
\item{coef}{this determines how far the plot ``whiskers'' extend out
  from the box.  If \code{coef} is positive, the whiskers extend to the
  most extreme data point which is no more than \code{coef} times the
  interquartile coef from the box.  A value of zero causes the whiskers
  to extend to the data extremes.}
}
\value{
This function is typically is called by \code{\link{boxplot}} to
gather the statistics necessary for producing box plots,
but may be invoked separately.
The value it returns is
a list with named components as follows:
\item{stats}{a vector of length 5, containing the extreme of the
  lower whisker, the lower ``hinge'', the median, the upper ``hinge''
  and the extreme of the upper whisker.}
\item{n}{the number of of non-\code{NA} observations in the sample.}
\item{conf}{the lower and upper extremes of the ``notch''.}
\item{out}{the values of any data points which lie beyond the
  extremes of the whiskers.}

Note that \code{$stats} and \code{$conf} are sorted \emph{in}creasingly,
  contrary to S, and that \code{$n} and \code{$out} include any
  \code{+- Inf} values.
}
\seealso{
\code{\link{fivenum}}, \code{\link{boxplot}}, \code{\link{bxp}}.
}
\examples{
str(boxplot.stats(1:100))
str(boxplot.stats(1:100, coef = 3))
str(boxplot.stats(1:100, coef = 0))

str(boxplot.stats(c(1:100,NA)))
str(boxplot.stats(c(1:100,-1:1/0)))
}
\keyword{dplot}
