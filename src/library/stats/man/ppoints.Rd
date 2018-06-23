% File src/library/stats/man/ppoints.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{ppoints}
\title{Ordinates for Probability Plotting}
\usage{
ppoints(n, a = if(n <= 10) 3/8 else 1/2)
}
\alias{ppoints}
\arguments{
  \item{n}{either the number of points generated or a vector of
    observations.}
  \item{a}{the offset fraction to be used; typically in \eqn{(0,1)}.}
}
\description{
  Generates the sequence of probability points
  \code{(1:m - a)/(m + (1-a)-a)}
  where \code{m} is either \code{n}, if \code{length(n)==1}, or
  \code{length(n)}.
}
\details{
  If \eqn{0 < a < 1}, the resulting values are within \eqn{(0,1)}
  (excluding boundaries).
  In any case, the resulting sequence is symmetric in \eqn{[0,1]}, i.e.,
  \code{p + rev(p) == 1}.

  \code{ppoints()} is used in \code{qqplot} and \code{qqnorm} to generate
  the set of probabilities at which to evaluate the inverse distribution.

  The choice of \code{a} follows the documentation of the function of the
  same name in Becker \emph{et al} (1988), and appears to have been
  motivated by results from Blom (1958) on approximations to expect normal
  order statistics (see also \code{\link{quantile}}).

  The probability points for the continuous sample quantile types 5 to 9
  (see \code{\link{quantile}}) can be obtained by taking \code{a} as,
  respectively, 1/2, 0, 1, 1/3, and 3/8.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Blom, G. (1958)
  \emph{Statistical Estimates and Transformed Beta Variables.}
  Wiley
}
\seealso{
  \code{\link{qqplot}}, \code{\link{qqnorm}}.
}
\examples{
ppoints(4) # the same as  ppoints(1:4)
ppoints(10)
ppoints(10, a = 1/2)

## Visualize including the fractions :
require(graphics)\dontshow{lNs <- loadedNamespaces()}
p.ppoints <- function(n, ..., add = FALSE, col = par("col")) {
  pn <- ppoints(n, ...)
  if(add)
      points(pn, pn, col = col)
  else {
      tit <- match.call(); tit[[1]] <- quote(ppoints)
      plot(pn,pn, main = deparse(tit), col=col,
           xlim = 0:1, ylim = 0:1, xaxs = "i", yaxs = "i")
      abline(0, 1, col = adjustcolor(1, 1/4), lty = 3)
  }
  if(!add && requireNamespace("MASS", quietly = TRUE))
    text(pn, pn, as.character(MASS::fractions(pn)),
         adj = c(0,0)-1/4, cex = 3/4, xpd = NA, col=col)
  abline(h = pn, v = pn, col = adjustcolor(col, 1/2), lty = 2, lwd = 1/2)
}

p.ppoints(4)
p.ppoints(10)
p.ppoints(10, a = 1/2)
p.ppoints(21)
p.ppoints(8) ; p.ppoints(8, a = 1/2, add=TRUE, col="tomato")
\dontshow{if(!any("MASS" == lNs)) unloadNamespace("MASS")}
}
\keyword{dplot}
\keyword{arith}
\keyword{distribution}
