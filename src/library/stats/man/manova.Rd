% File src/library/stats/man/manova.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{manova}
\alias{manova}
\title{Multivariate Analysis of Variance}
\description{
  A class for the multivariate analysis of variance.
}
\usage{
manova(\dots)
}
\arguments{
  \item{\dots}{Arguments to be passed to \code{\link{aov}}.}
}
\details{
  Class \code{"manova"} differs from class \code{"aov"} in selecting a
  different \code{summary} method.  Function \code{manova} calls
  \code{\link{aov}} and then add class \code{"manova"} to the result
  object for each stratum.
}
\value{
  See \code{\link{aov}} and the comments in \sQuote{Details} here.
}
% \note{
%   \code{manova} does not support multistratum analysis of variance, so
%   the formula should not include an \code{Error} term.
% }
\references{
  Krzanowski, W. J. (1988) \emph{Principles of Multivariate Analysis. A
    User's Perspective.} Oxford.

  Hand, D. J. and Taylor, C. C.  (1987)
  \emph{Multivariate Analysis of Variance and Repeated Measures.}
  Chapman and Hall.
}
\examples{
## Set orthogonal contrasts.
op <- options(contrasts = c("contr.helmert", "contr.poly"))

## Fake a 2nd response variable
npk2 <- within(npk, foo <- rnorm(24))
( npk2.aov <- manova(cbind(yield, foo) ~ block + N*P*K, npk2) )
summary(npk2.aov)

( npk2.aovE <- manova(cbind(yield, foo) ~  N*P*K + Error(block), npk2) )
summary(npk2.aovE)


}
\seealso{
  \code{\link{aov}}, \code{\link{summary.manova}}, the latter containing
  examples.
}
\keyword{models}
