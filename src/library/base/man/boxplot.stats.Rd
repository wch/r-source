\name{boxplot.stats}
\title{Box Plot Statistics}
\usage{
boxplot.stats(x, coef = 1.5, do.conf=TRUE, do.out=TRUE)
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
    to extend to the data extremes (and no outliers be returned).}
  \item{do.conf,do.out}{logicals; if \code{FALSE}, the \code{conf} or
    \code{out} component respectively will be empty in the result.}
}
\description{
  This function is typically called by \code{\link{boxplot}} to
  gather the statistics necessary for producing box plots,
  but may be invoked separately.
}
\value{
  List with named components as follows:
  \item{stats}{a vector of length 5, containing the extreme of the
    lower whisker, the lower ``hinge'', the median, the upper ``hinge''
    and the extreme of the upper whisker.}
  \item{n}{the number of of non-\code{NA} observations in the sample.}
  \item{conf}{the lower and upper extremes of the ``notch'' (\code{if(do.conf)}).}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers (\code{if(do.out)}).}

  Note that \code{$stats} and \code{$conf} are sorted in \emph{in}creasing
  order, unlike S, and that \code{$n} and \code{$out} include any
  \code{+- Inf} values.
}
\seealso{
  \code{\link{fivenum}}, \code{\link{boxplot}}, \code{\link{bxp}}.
}
\examples{
x <- c(1:100, 1000)
str(b1 <- boxplot.stats(x))
str(b2 <- boxplot.stats(x, do.conf=FALSE, do.out=FALSE))
stopifnot(b1 $ stats == b2 $ stats) # do.out=F is still robust
str(boxplot.stats(x, coef = 3, do.conf=FALSE))
## no outlier treatment:
str(boxplot.stats(x, coef = 0))

str(boxplot.stats(c(x, NA))) # slight change : n + 1
str(r <- boxplot.stats(c(x, -1:1/0)))
stopifnot(r$out == c(1000, -Inf, Inf))
}
\keyword{dplot}
