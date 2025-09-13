% File src/library/base/man/jitter.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{jitter}
\alias{jitter}
\title{\sQuote{Jitter} (Add Noise) to Numbers}
\usage{
jitter(x, factor = 1, amount = NULL)
}
\arguments{
  \item{x}{numeric vector to which \emph{jitter} should be added.}
  \item{factor}{numeric; used in calculation of \code{amount} when
   the provided \code{amount} is 0 or \code{NULL}.}
  \item{amount}{number or \code{NULL}; when positive, specifies
    the amount of jitter; see Details, also for other cases.}
}
\description{
  Add a small amount of noise (\dQuote{jitter}) to a numeric vector.
}
\value{
  \code{jitter(x, \dots)} returns a numeric of the same length as
  \code{x}, but with an \code{amount} of noise added in order to break
  ties.
}
\details{
  The result of \code{r <-  x + amount * runif(n, -1, 1)}
  where \code{n <- length(x)}.

  When \code{amount} is \code{NULL} (the default)
  \code{amount <- factor * d/5} where \emph{d} is about the smallest
  difference between adjacent unique (up to fuzz) \code{x} values.

  If \code{amount == 0}, \code{amount <- factor * (max(x) - min(x))/50},
  (using only finite \code{x} values), which is the same as in S and the
  references.
  In the above cases, the final \code{amount} is always ensured to be positive.

  The short \code{jitter()} function is itself a succinct definition.
}
\references{
  \bibinfo{R:Chambers+Cleveland+Kleiner:1983}{note}{(Figures 2.8, 4.22, 5.4.)}
  \bibshow{R:Chambers+Cleveland+Kleiner:1983, R:Chambers+Hastie:1992}
}
\author{Werner Stahel and Martin Maechler, ETH Zurich}
\seealso{
  \code{\link{rug}} which you may want to combine with \code{jitter}.
}
\examples{
round(jitter(c(rep(1, 3), rep(1.2, 4), rep(3, 3))), 3)
## These two had failed with S-plus 3.x: max(x) - min(x) == 0
jitter(rep(0, 7))
jitter(rep(10000, 5))
}
\keyword{dplot}
\keyword{utilities}
