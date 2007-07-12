\name{qqnorm}
\title{Quantile-Quantile Plots}
\usage{
qqnorm(y, \dots)
\method{qqnorm}{default}(y, ylim, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE, \dots)

qqline(y, datax = FALSE, \dots)

qqplot(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
       ylab = deparse(substitute(y)), \dots)
}
\alias{qqnorm}
\alias{qqnorm.default}
\alias{qqplot}
\alias{qqline}

\arguments{
  \item{x}{The first sample for \code{qqplot}.}
  \item{y}{The second or only data sample.}
  \item{xlab, ylab, main}{plot labels.  The \code{xlab} and \code{ylab}
    refer to the y and x axes respectively if \code{datax = TRUE}.}
  \item{plot.it}{logical. Should the result be plotted?}
  \item{datax}{logical. Should data values be on the x-axis?}
  \item{ylim, \dots}{graphical parameters.}
}
\description{
  \code{qqnorm} is a generic function the default method of which
  produces a normal QQ plot of the values in \code{y}.
  \code{qqline} adds a line to a normal quantile-quantile plot which
  passes through the first and third quartiles.

  \code{qqplot} produces a QQ plot of two datasets.

  Graphical parameters may be given as arguments to \code{qqnorm},
  \code{qqplot} and \code{qqline}.
}
\value{
  For \code{qqnorm} and \code{qqplot}, a list with components
  \item{x}{The x coordinates of the points that were/would be plotted}
  \item{y}{The original \code{y} vector, i.e., the corresponding y
    coordinates \emph{including \code{\link{NA}}s}.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{ppoints}}, used by \code{qqnorm} to generate
  approximations to expected order statistics for a normal distribution.
}
\examples{
require(graphics)

y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))

qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
}
\keyword{hplot}
\keyword{distribution}
