\name{jitter}
\alias{jitter}
\title{Add `Jitter' (Noise) to Numbers}
\usage{
jitter(x, factor=1, amount = NULL)
}
\arguments{
  \item{x}{numeric vector to which \emph{jitter} should be added.}
  \item{factor}{numeric }
  \item{amount}{numeric; if positive, used as \emph{amount} (see below),
    otherwise, if \code{= 0} the default is \code{factor * z/50}.

    Default (\code{NULL}): \code{factor * d/5} where \code{d} is about
    the smallest difference between \code{x} values.}
}
\description{
  Add a small amount of noise to a numeric vector.
}
\value{
  \code{jitter(x,\dots)} returns a numeric of the same length as
  \code{x}, but with an \code{amount} of noise added in order to break
  ties. 
}
\details{
  The result, say \code{r}, is \code{r <- x + runif(n, -a, a)}
  where \code{n <- length(x)} and \code{a} is the \code{amount}
  argument (if specified).

  Let \code{z <- max(x) - min(x)} (assuming the usual case).
  The amount \code{a} to be added is either provided as \emph{positive}
  argument \code{amount} or otherwise computed from \code{z}, as
  follows:

  If \code{amount == 0}, we set \code{a <- factor * z/50} (same as S).

  If \code{amount} is \code{NULL} (\emph{default}), we set
  \code{a <- factor * d/5} where \emph{d} is the smallest
  difference between adjacent unique (apart from fuzz) \code{x} values.
}
\references{
  Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P.A. (1983)
  \emph{Graphical Methods for Data Analysis.} Wadsworth; figures 2.8,
  4.22, 5.4.

  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth \& Brooks/Cole.
}
\author{Werner Stahel and Martin Maechler, ETH Zurich}
\seealso{
  \code{\link{rug}} which you may want to combine with \code{jitter}.
}
\examples{
round(jitter(c(rep(1,3),  rep(1.2, 4), rep(3,3))), 3)
## These two 'fail' with S-plus 3.x:
jitter(rep(0, 7))
jitter(rep(10000,5))
}
\keyword{dplot}
\keyword{utilities}
