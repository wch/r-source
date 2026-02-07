% File src/library/grDevices/man/nclass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{nclass}
\alias{nclass.Sturges}
\alias{nclass.scott}
\alias{nclass.FD}
\title{Compute the Number of Classes for a Histogram}
\description{
  Compute the number of classes for a histogram, notably \code{\link{hist}()}.
}
\usage{
nclass.Sturges(x)
nclass.scott(x)
nclass.FD(x, digits = 5)
}
\arguments{
  \item{x}{a data vector.}
  \item{digits}{number of \emph{significant} digits to keep when rounding
    \code{x} before the \code{\link{IQR}} computation; see \sQuote{Details} below.}
}
\value{
  The suggested number of classes.
}
\details{
  \code{nclass.Sturges} uses \I{Sturges}' formula, implicitly basing bin
  sizes on the range of the data.

  \code{nclass.scott} uses Scott's choice for a normal distribution based on
  the estimate of the standard error, unless that is zero where it
  returns \code{1}.

  \code{nclass.FD} uses the \I{Freedman}-\I{Diaconis} choice based on the
  inter-quartile range (\code{\link{IQR}(signif(x, digits))}) unless that's
  zero where it uses increasingly more extreme symmetric quantiles up to
  c(1,511)/512 and if that difference is still zero, reverts to using
  Scott's choice.  The default of \code{digits = 5} was chosen after a few
  experiments, but may be too low for some situations, see \PR{17274}.
}
\seealso{
  \code{\link{hist}} and \code{\link[MASS]{truehist}} (package
  \CRANpkg{MASS});  \code{\link[KernSmooth]{dpih}} (package
  \CRANpkg{KernSmooth}) for a plugin bandwidth proposed by
  \bibcitet{R:Wand:1997}.
}
\references{
  \bibinfo{R:Venables+Ripley:2002}{note}{Page 112}
  \bibshow{R:Venables+Ripley:2002,
    R:Freedman+Diaconis:1981,
    R:Scott:1979,
    R:Scott:1992,
    R:Sturges:1926,
    R:Wand:1997}
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
