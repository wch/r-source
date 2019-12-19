% File src/library/stats/man/qqnorm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{qqnorm}
\title{Quantile-Quantile Plots}
\usage{
qqnorm(y, \dots)
\method{qqnorm}{default}(y, ylim, main = "Normal Q-Q Plot",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles",
       plot.it = TRUE, datax = FALSE, \dots)

qqline(y, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7, \dots)

qqplot(x, y, plot.it = TRUE,
       xlab = deparse1(substitute(x)),
       ylab = deparse1(substitute(y)), \dots)
}
\alias{qqnorm}
\alias{qqnorm.default}
\alias{qqplot}
\alias{qqline}
\concept{normal probability plot} % PR#12095

\arguments{
  \item{x}{The first sample for \code{qqplot}.}
  \item{y}{The second or only data sample.}
  \item{xlab, ylab, main}{plot labels.  The \code{xlab} and \code{ylab}
    refer to the y and x axes respectively if \code{datax = TRUE}.}
  \item{plot.it}{logical. Should the result be plotted?}
  \item{datax}{logical. Should data values be on the x-axis?}
  \item{distribution}{quantile function for reference theoretical distribution.}
  \item{probs}{numeric vector of length two, representing probabilities.
    Corresponding quantile pairs define the line drawn.}
  \item{qtype}{the \code{type} of quantile computation used in \code{\link{quantile}}.}

  \item{ylim, \dots}{graphical parameters.}
}
\description{
  \code{qqnorm} is a generic function the default method of which
  produces a normal QQ plot of the values in \code{y}.
  \code{qqline} adds a line to a \dQuote{theoretical}, by default
  normal, quantile-quantile plot which passes through the \code{probs}
  quantiles, by default the first and third quartiles.

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
  Wadsworth & Brooks/Cole.
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

## "QQ-Chisquare" : --------------------------
y <- rchisq(500, df = 3)
## Q-Q plot for Chi^2 data against true theoretical distribution:
qqplot(qchisq(ppoints(500), df = 3), y,
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
qqline(y, distribution = function(p) qchisq(p, df = 3),
       probs = c(0.1, 0.6), col = 2)
mtext("qqline(*, dist = qchisq(., df=3), prob = c(0.1, 0.6))")
## (Note that the above uses ppoints() with a = 1/2, giving the
## probability points for quantile type 5: so theoretically, using
## qqline(qtype = 5) might be preferable.) 
}
\keyword{hplot}
\keyword{distribution}
