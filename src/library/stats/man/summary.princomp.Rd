% File src/library/stats/man/summary.princomp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{summary.princomp}
\alias{summary.princomp}
\alias{print.summary.princomp}
\title{Summary method for Principal Components Analysis}
\usage{
\method{summary}{princomp}(object, loadings = FALSE, cutoff = 0.1, \dots)

\method{print}{summary.princomp}(x, digits = 3, loadings = x$print.loadings,
      cutoff = x$cutoff, \dots)
}
\arguments{
  \item{object}{an object of class \code{"princomp"}, as
    from \code{princomp()}.}
  \item{loadings}{logical. Should loadings be included?}
  \item{cutoff}{numeric. Loadings below this cutoff in absolute value
    are shown as blank in the output.}
  \item{x}{an object of class "summary.princomp".}
  \item{digits}{the number of significant digits to be used in listing
    loadings.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\description{
  The \code{\link{summary}} method for class \code{"princomp"}.
}
\value{
  \code{object} with additional components \code{cutoff} and
  \code{print.loadings}.
}
\seealso{
  \code{\link{princomp}}
}
\examples{
summary(pc.cr <- princomp(USArrests, cor = TRUE))
## The signs of the loading columns are arbitrary
print(summary(princomp(USArrests, cor = TRUE),
              loadings = TRUE, cutoff = 0.2), digits = 2)
}
\keyword{multivariate}
