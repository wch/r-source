% File src/library/stats/man/runmed.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{runmed}
\title{Running Medians -- Robust Scatter Plot Smoothing}
\alias{runmed}
\encoding{UTF-8}
\description{
  Compute running medians of odd span.  This is the \sQuote{most robust}
  scatter plot smoothing possible.  For efficiency (and historical
  reason), you can use one of two different algorithms giving identical
  results.
}
\usage{
runmed(x, k, endrule = c("median", "keep", "constant"),
       algorithm = NULL, print.level = 0)
}
\arguments{
  \item{x}{numeric vector, the \sQuote{dependent} variable to be
    smoothed.}
  \item{k}{integer width of median window; must be odd.  Turlach had a
    default of \code{k <- 1 + 2 * min((n-1)\%/\% 2, ceiling(0.1*n))}.
    Use \code{k = 3} for \sQuote{minimal} robust smoothing eliminating
    isolated outliers.}
  \item{endrule}{character string indicating how the values at the
    beginning and the end (of the data) should be treated.
    Can be abbreviated.  Possible values are:
    \describe{
      \item{\code{"keep"}}{keeps the first and last \eqn{k_2}{k2} values
        at both ends, where \eqn{k_2}{k2} is the half-bandwidth
        \code{k2 = k \%/\% 2},
        i.e., \code{y[j] = x[j]} for \eqn{j \in \{1,\ldots,k_2;
          n-k_2+1,\ldots,n\}}{j = 1, \dots, k2 and (n-k2+1), \dots, n};}
      \item{\code{"constant"}}{copies \code{median(y[1:k2])} to the first
        values and analogously for the last ones making the smoothed ends
        \emph{constant};}
      \item{\code{"median"}}{the default, smooths the ends by using
        symmetrical medians of subsequently smaller bandwidth, but for
        the very first and last value where Tukey's robust end-point
        rule is applied, see \code{\link{smoothEnds}}.}
    }
  }
  \item{algorithm}{character string (partially matching \code{"Turlach"} or
    \code{"Stuetzle"}) or the default \code{NULL}, specifying which algorithm
    should be applied.  The default choice depends on \code{n = length(x)}
    and \code{k} where \code{"Turlach"} will be used for larger problems.}
  \item{print.level}{integer, indicating verboseness of algorithm;
    should rarely be changed by average users.}
}
\value{vector of smoothed values of the same length as \code{x} with an
  \code{\link{attr}}ibute \code{k} containing (the \sQuote{oddified})
  \code{k}.
}
\details{
  Apart from the end values, the result \code{y = runmed(x, k)} simply has
  \code{y[j] = median(x[(j-k2):(j+k2)])} (\code{k = 2*k2+1}), computed very
  efficiently.

  The two algorithms are internally entirely different:
  \describe{
    \item{\code{"Turlach"}}{is the \enc{Härdle}{Haerdle}--Steiger
      algorithm (see Ref.) as implemented by Berwin Turlach.
      A tree algorithm is used, ensuring performance \eqn{O(n \log
        k)}{O(n * log(k))} where \code{n = length(x)} which is
      asymptotically optimal.}
    \item{\code{"Stuetzle"}}{is the (older) Stuetzle--Friedman implementation
      which makes use of median \emph{updating} when one observation
      enters and one leaves the smoothing window.  While this performs as
      \eqn{O(n \times k)}{O(n * k)} which is slower asymptotically, it is
      considerably faster for small \eqn{k} or \eqn{n}.}
  }

  Currently long vectors are only supported for \code{algorithm = "Steutzle"}.
}
\references{
  \enc{Härdle}{Haerdle}, W. and Steiger, W. (1995)
  [Algorithm AS 296] Optimal median smoothing,
  \emph{Applied Statistics} \bold{44}, 258--264.

  Jerome H. Friedman and Werner Stuetzle (1982)
  \emph{Smoothing of Scatterplots};
  Report, Dep. Statistics, Stanford U., Project Orion 003.

  Martin Maechler (2003)
  Fast Running Medians: Finite Sample and Asymptotic Optimality;
  working paper available from the author.
}
\author{Martin Maechler \email{maechler@stat.math.ethz.ch},
  based on Fortran code from Werner Stuetzle and S-PLUS and C code from
  Berwin Turlach.
}
\seealso{
  \code{\link{smoothEnds}} which implements Tukey's end point rule and
  is called by default from \code{runmed(*, endrule = "median")}.
  \code{\link{smooth}} uses running
  medians of 3 for its compound smoothers.
}
\examples{
require(graphics)

utils::example(nhtemp)
myNHT <- as.vector(nhtemp)
myNHT[20] <- 2 * nhtemp[20]
plot(myNHT, type = "b", ylim = c(48, 60), main = "Running Medians Example")
lines(runmed(myNHT, 7), col = "red")

## special: multiple y values for one x
plot(cars, main = "'cars' data and runmed(dist, 3)")
lines(cars, col = "light gray", type = "c")
with(cars, lines(speed, runmed(dist, k = 3), col = 2))
%% FIXME: Show how to do it properly ! tapply(*, unique(.), median)

## nice quadratic with a few outliers
y <- ys <- (-20:20)^2
y [c(1,10,21,41)] <- c(150, 30, 400, 450)
all(y == runmed(y, 1)) # 1-neighbourhood <==> interpolation
plot(y) ## lines(y, lwd = .1, col = "light gray")
lines(lowess(seq(y), y, f = 0.3), col = "brown")
lines(runmed(y, 7), lwd = 2, col = "blue")
lines(runmed(y, 11), lwd = 2, col = "red")

## Lowess is not robust
y <- ys ; y[21] <- 6666 ; x <- seq(y)
col <- c("black", "brown","blue")
plot(y, col = col[1])
lines(lowess(x, y, f = 0.3), col = col[2])
%% predict(loess(y ~ x, span = 0.3, degree=1, family = "symmetric"))
%% gives 6-line warning but does NOT break down
lines(runmed(y, 7),      lwd = 2, col = col[3])
legend(length(y),max(y), c("data", "lowess(y, f = 0.3)", "runmed(y, 7)"),
       xjust = 1, col = col, lty = c(0, 1, 1), pch = c(1,NA,NA))
}
\keyword{smooth}
\keyword{robust}
