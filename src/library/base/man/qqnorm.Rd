\name{qqnorm}
\title{Normal Quantile-Quantile Plots}
\usage{
qqnorm(y, \dots)
qqline(y, \dots)
qqplot(x,y, plot.it = T, \dots)
}
\alias{qqnorm}
\alias{qqplot}
\alias{qqline}
\description{
\code{qqnorm} produces a normal QQ plot of the values in \code{y}.
\code{qqline} adds a line to a normal quantile-quantile plot
which passes through the quartiles.

Graphical parameters may be given as arguments to \code{qqnorm}, \code{qqnorm}
and \code{qqline}.
}
\seealso{
\code{\link{ppoints}}.
}
\examples{
y <- rt(200, df=5)
qqnorm(y); qqline(y, col=2)
qqplot(y, rt(300, df=5))
data(precip)
qqnorm(precip, ylab = "Precipitation [in/yr] for 70 US cities")
}
