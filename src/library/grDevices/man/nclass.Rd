% File src/library/grDevices/man/nclass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{nclass}
\alias{nclass.Sturges}
\alias{nclass.scott}
\alias{nclass.FD}
\encoding{UTF-8}
\title{Compute the Number of Classes for a Histogram}
\description{
  Compute the number of classes for a histogram.
}
\usage{
nclass.Sturges(x)
nclass.scott(x)
nclass.FD(x)
}
\arguments{
  \item{x}{
    A data vector.
  }
}
\value{
  The suggested number of classes.
}
\details{
  \code{nclass.Sturges} uses Sturges' formula, implicitly basing bin
  sizes on the range of the data.

  \code{nclass.scott} uses Scott's choice for a normal distribution based on
  the estimate of the standard error, unless that is zero where it
  returns \code{1}.

  \code{nclass.FD} uses the Freedman-Diaconis choice based on the
  inter-quartile range (\code{\link{IQR}}) unless that's zero where it
  reverts to \code{\link{mad}(x, constant = 2)} and when that is \eqn{0}
  as well, returns \code{1}.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S-PLUS.}
  Springer, page 112.

  Freedman, D. and Diaconis, P. (1981)
  On the histogram as a density estimator: \eqn{L_2} theory.
  \emph{Zeitschrift \enc{für}{fuer} Wahrscheinlichkeitstheorie
    und verwandte Gebiete} \bold{57}, 453--476.

  Scott, D. W. (1979) On optimal and data-based histograms.
  \emph{Biometrika} \bold{66}, 605--610.

  Scott, D. W. (1992)
  \emph{Multivariate Density Estimation. Theory, Practice, and
    Visualization}. Wiley.

  Sturges, H. A. (1926)  The choice of a class interval.
  \emph{Journal of the American Statistical Association} \bold{21}, 65--66.
}
\seealso{
  \code{\link{hist}} and \code{\link[MASS]{truehist}} (package
  \CRANpkg{MASS});  \code{\link[KernSmooth]{dpih}} (package
  \CRANpkg{KernSmooth}) for a plugin bandwidth proposed by Wand(1995).
}
\examples{
set.seed(1)
x <- stats::rnorm(1111)
nclass.Sturges(x)

## Compare them:
NC <- function(x) c(Sturges = nclass.Sturges(x),
      Scott = nclass.scott(x), FD = nclass.FD(x))
NC(x)
onePt <- rep(1, 11)
NC(onePt) # no longer gives NaN
}
\keyword{univar}
