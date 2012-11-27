% File src/library/stats/man/density.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{density}
\alias{density}
\alias{density.default}
\alias{print.density}
\title{Kernel Density Estimation}
\usage{
density(x, \dots)
\method{density}{default}(x, bw = "nrd0", adjust = 1,
        kernel = c("gaussian", "epanechnikov", "rectangular",
                   "triangular", "biweight",
                   "cosine", "optcosine"),
        weights = NULL, window = kernel, width,
        give.Rkern = FALSE,
        n = 512, from, to, cut = 3, na.rm = FALSE, \dots)
}
\arguments{
  \item{x}{the data from which the estimate is to be computed.}

  \item{bw}{the smoothing bandwidth to be used.  The kernels are scaled
    such that this is the standard deviation of the smoothing kernel.
    (Note this differs from the reference books cited below, and from S-PLUS.)

    \code{bw} can also be a character string giving a rule to choose the
    bandwidth.  See \code{\link{bw.nrd}}. \cr
    The default, \code{"nrd0"}, has remained the default for historical
    and compatibility reasons, rather than as a general recommendation,
    where e.g., \code{"SJ"} would rather fit, see also V&R (2002).

    The specified (or computed) value of \code{bw} is multiplied by
    \code{adjust}.
  }
  \item{adjust}{the bandwidth used is actually \code{adjust*bw}.
    This makes it easy to specify values like \sQuote{half the default}
    bandwidth.}

  \item{kernel, window}{a character string giving the smoothing kernel
    to be used. This must be one of \code{"gaussian"},
    \code{"rectangular"}, \code{"triangular"}, \code{"epanechnikov"},
    \code{"biweight"}, \code{"cosine"} or \code{"optcosine"}, with default
    \code{"gaussian"}, and may be abbreviated to a unique prefix (single
    letter).

    \code{"cosine"} is smoother than \code{"optcosine"}, which is the
    usual \sQuote{cosine} kernel in the literature and almost MSE-efficient.
    However, \code{"cosine"} is the version used by S.
  }

  \item{weights}{numeric vector of non-negative observation weights,
    hence of same length as \code{x}. The default \code{NULL} is
    equivalent to \code{weights = rep(1/nx, nx)} where \code{nx} is the
    length of (the finite entries of) \code{x[]}.}

  \item{width}{this exists for compatibility with S; if given, and
    \code{bw} is not, will set \code{bw} to \code{width} if this is a
    character string, or to a kernel-dependent multiple of \code{width}
    if this is numeric.}

  \item{give.Rkern}{logical; if true, \emph{no} density is estimated, and
    the \sQuote{canonical bandwidth} of the chosen \code{kernel} is returned
    instead.}

  \item{n}{the number of equally spaced points at which the density is
    to be estimated.  When \code{n > 512}, it is rounded up to a power
    of 2 during the calculations (as \code{\link{fft}} is used) and the
    final result is interpolated by \code{\link{approx}}.  So it almost
    always makes sense to specify \code{n} as a power of two.
  }

  \item{from,to}{the left and right-most points of the grid at which the
    density is to be estimated; the defaults are \code{cut * bw} outside
    of \code{range(x)}.}

  \item{cut}{by default, the values of \code{from} and \code{to} are
    \code{cut} bandwidths beyond the extremes of the data.  This allows
    the estimated density to drop to approximately zero at the extremes.}

  \item{na.rm}{logical; if \code{TRUE}, missing values are removed
    from \code{x}. If \code{FALSE} any missing values cause an error.}
  \item{\dots}{further arguments for (non-default) methods.}
}
\description{
  The (S3) generic function \code{density} computes kernel density
  estimates.  Its default method does so with the given kernel and
  bandwidth for univariate observations.
}
\details{
  The algorithm used in \code{density.default} disperses the mass of the
  empirical distribution function over a regular grid of at least 512
  points and then uses the fast Fourier transform to convolve this
  approximation with a discretized version of the kernel and then uses
  linear approximation to evaluate the density at the specified points.

  The statistical properties of a kernel are determined by
  \eqn{\sigma^2_K = \int t^2 K(t) dt}{sig^2 (K) = int(t^2 K(t) dt)}
  which is always \eqn{= 1} for our kernels (and hence the bandwidth
  \code{bw} is the standard deviation of the kernel) and
  \eqn{R(K) = \int K^2(t) dt}{R(K) = int(K^2(t) dt)}.\cr
  MSE-equivalent bandwidths (for different kernels) are proportional to
  \eqn{\sigma_K R(K)}{sig(K) R(K)} which is scale invariant and for our
  kernels equal to \eqn{R(K)}.  This value is returned when
  \code{give.Rkern = TRUE}.  See the examples for using exact equivalent
  bandwidths.

  Infinite values in \code{x} are assumed to correspond to a point mass at
  \code{+/-Inf} and the density estimate is of the sub-density on
  \code{(-Inf, +Inf)}.
}
\value{
  If \code{give.Rkern} is true, the number \eqn{R(K)}, otherwise
  an object with class \code{"density"} whose
  underlying structure is a list containing the following components.
  \item{x}{the \code{n} coordinates of the points where the density is
    estimated.}
  \item{y}{the estimated density values.  These will be non-negative,
    but can be zero.}
  \item{bw}{the bandwidth used.}
  \item{n}{the sample size after elimination of missing values.}
  \item{call}{the call which produced the result.}
  \item{data.name}{the deparsed name of the \code{x} argument.}
  \item{has.na}{logical, for compatibility (always \code{FALSE}).}

  The \code{print} method reports \code{\link{summary}} values on the
  \code{x} and \code{y} components.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole (for S version).

  Scott, D. W. (1992)
  \emph{Multivariate Density Estimation. Theory, Practice and Visualization}.
  New York: Wiley.

  Sheather, S. J. and Jones M. C. (1991)
  A reliable data-based bandwidth selection method for kernel density
  estimation.
  \emph{J. Roy. Statist. Soc.} \bold{B}, 683--690.

  Silverman, B. W. (1986)
  \emph{Density Estimation}.
  London: Chapman and Hall.

  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S}.
  New York: Springer.
}
\seealso{
  \code{\link{bw.nrd}},
  \code{\link{plot.density}}, \code{\link{hist}}.
}
\examples{
require(graphics)

plot(density(c(-20, rep(0,98), 20)), xlim = c(-4, 4))  # IQR = 0

# The Old Faithful geyser data
d <- density(faithful$eruptions, bw = "sj")
d
plot(d)

plot(d, type = "n")
polygon(d, col = "wheat")

## Missing values:
x <- xx <- faithful$eruptions
x[i.out <- sample(length(x), 10)] <- NA
doR <- density(x, bw = 0.15, na.rm = TRUE)
lines(doR, col = "blue")
points(xx[i.out], rep(0.01, 10))

## Weighted observations:
fe <- sort(faithful$eruptions) # has quite a few non-unique values
## use 'counts / n' as weights:
dw <- density(unique(fe), weights = table(fe)/length(fe), bw = d$bw)
utils::str(dw) ## smaller n: only 126, but identical estimate:
stopifnot(all.equal(d[1:3], dw[1:3]))

## simulation from a density() fit:
# a kernel density fit is an equally-weighted mixture.
fit <- density(xx)
N <- 1e6
x.new <- rnorm(N, sample(xx, size = N, replace = TRUE), fit$bw)
plot(fit)
lines(density(x.new), col = "blue")


(kernels <- eval(formals(density.default)$kernel))

## show the kernels in the R parametrization
plot (density(0, bw = 1), xlab = "",
      main = "R's density() kernels with bw = 1")
for(i in 2:length(kernels))
   lines(density(0, bw = 1, kernel =  kernels[i]), col = i)
legend(1.5,.4, legend = kernels, col = seq(kernels),
       lty = 1, cex = .8, y.intersp = 1)

## show the kernels in the S parametrization
plot(density(0, from = -1.2, to = 1.2, width = 2, kernel = "gaussian"),
     type = "l", ylim = c(0, 1), xlab = "",
     main = "R's density() kernels with width = 1")
for(i in 2:length(kernels))
   lines(density(0, width = 2, kernel =  kernels[i]), col = i)
legend(0.6, 1.0, legend = kernels, col = seq(kernels), lty = 1)

##-------- Semi-advanced theoretic from here on -------------
%% i.e. "secondary example" in a new help system ...

(RKs <- cbind(sapply(kernels,
                     function(k) density(kernel = k, give.Rkern = TRUE))))
100*round(RKs["epanechnikov",]/RKs, 4) ## Efficiencies

bw <- bw.SJ(precip) ## sensible automatic choice
plot(density(precip, bw = bw),
     main = "same sd bandwidths, 7 different kernels")
for(i in 2:length(kernels))
   lines(density(precip, bw = bw, kernel = kernels[i]), col = i)

## Bandwidth Adjustment for "Exactly Equivalent Kernels"
h.f <- sapply(kernels, function(k)density(kernel = k, give.Rkern = TRUE))
(h.f <- (h.f["gaussian"] / h.f)^ .2)
## -> 1, 1.01, .995, 1.007,... close to 1 => adjustment barely visible..

plot(density(precip, bw = bw),
     main = "equivalent bandwidths, 7 different kernels")
for(i in 2:length(kernels))
   lines(density(precip, bw = bw, adjust = h.f[i], kernel = kernels[i]),
         col = i)
legend(55, 0.035, legend = kernels, col = seq(kernels), lty = 1)
}
\keyword{distribution}
\keyword{smooth}
