\name{qqnorm}
\title{Quantile-Quantile Plots}
\usage{
qqnorm(y, ylim, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles",
       ylab = "Sample Quantiles", plot.it = TRUE, \dots)
qqline(y, \dots)
qqplot(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
       ylab = deparse(substitute(y)), \dots)
}
\alias{qqnorm}
\alias{qqplot}
\alias{qqline}

\arguments{
 \item{x}{The first sample for \code{qqplot}.}
 \item{y}{The second or only data sample.}
}
\description{
  \code{qqnorm} produces a normal QQ plot of the values in \code{y}.
  \code{qqline} adds a line to a normal quantile-quantile plot which
  passes through the first and third quartiles.

  \code{qqplot} produces a QQ plot of two datasets.

  Graphical parameters may be given as arguments to \code{qqnorm},
  \code{qqplot} and \code{qqline}.
}
\value{
  For \code{qqnorm} and \code{qqplot}, a list with components
  \item{x}{The x coordinates of the points that were/would be plotted}
  \item{y}{The corresponding y coordinates}
  }
\seealso{
\code{\link{ppoints}}.
}
\examples{
y <- rt(200, df = 5)
qqnorm(y); qqline(y, col = 2)
qqplot(y, rt(300, df = 5))
data(precip)
qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
}
\keyword{hplot}
\keyword{distribution}
