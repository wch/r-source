% File src/library/stats/man/screeplot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{screeplot}
\alias{screeplot}
\alias{screeplot.default}
\title{Screeplots}
\usage{
\method{screeplot}{default}(x, npcs = min(10, length(x$sdev)),
          type = c("barplot", "lines"),
          main = deparse(substitute(x)), \dots)
}
\arguments{
  \item{x}{an object containing a \code{sdev} component, such as that
    returned by \code{\link{princomp}()} and \code{\link{prcomp}()}.}
  \item{npcs}{the number of components to be plotted.}
  \item{type}{the type of plot.  Can be abbreviated.}
  \item{main, \dots}{graphics parameters.}
}
\description{
  \code{screeplot.default} plots the variances against the number of the
  principal component. This is also the \code{plot} method for classes
  \code{"princomp"} and \code{"prcomp"}.
}
\references{
  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
  \emph{Multivariate Analysis}, London: Academic Press.

  Venables, W. N. and B. D. Ripley (2002).
  \emph{Modern Applied Statistics with S}, Springer-Verlag.
}
\seealso{
  \code{\link{princomp}} and \code{\link{prcomp}}.
}
\examples{
require(graphics)

## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
(pc.cr <- princomp(USArrests, cor = TRUE))  # inappropriate
screeplot(pc.cr)

fit <- princomp(covmat = Harman74.cor)
screeplot(fit)
screeplot(fit, npcs = 24, type = "lines")
}
\keyword{multivariate}
