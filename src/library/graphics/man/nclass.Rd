\name{nclass}
\alias{nclass.Sturges}
\alias{nclass.scott}
\alias{nclass.FD}
\encoding{latin1}
\title{
Compute the Number of Classes for a Histogram
}
\description{
  Compute the number of classes for a histogram, for use internally in
  \code{\link{hist}}.
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
  the estimate of the standard error.

  \code{nclass.FD} uses the
  Freedman-Diaconis choice based on the inter-quartile range.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S-PLUS.}
  Springer, page 112.

  Freedman, D. and Diaconis, P. (1981)
  On the histogram as a density estimator: \eqn{L_2} theory.
  \emph{Zeitschrift für Wahrscheinlichkeitstheorie und verwandte
    Gebiete} \bold{57}, 453--476.

  Scott, D. W. (1979) On optimal and data-based histograms.
  \emph{Biometrika} \bold{66}, 605--610.

  Scott, D. W. (1992)
  \emph{Multivariate Density Estimation. Theory, Practice, and
    Visualization}. Wiley.
}
\seealso{
  \code{\link{hist}}
}
\keyword{univar}
