% File src/library/stats/man/quantile.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{quantile}
\title{Sample Quantiles}
\alias{quantile}
\alias{quantile.default}
\description{
  The generic function \code{quantile} produces sample quantiles
  corresponding to the given probabilities.
  The smallest observation corresponds to a probability of 0 and the
  largest to a probability of 1.
}
\usage{
quantile(x, \dots)

\method{quantile}{default}(x, probs = seq(0, 1, 0.25), na.rm = FALSE,
         names = TRUE, type = 7, \dots)
}
\arguments{
  \item{x}{numeric vector whose sample quantiles are wanted, or an
    object of a class for which a method has been defined (see also
    \sQuote{details}). \code{\link{NA}} and \code{NaN} values are not
    allowed in numeric vectors unless \code{na.rm} is \code{TRUE}.}
  \item{probs}{numeric vector of probabilities with values in
    \eqn{[0,1]}.  (Values up to \samp{2e-14} outside that
    range are accepted and moved to the nearby endpoint.)}
  \item{na.rm}{logical; if true, any \code{\link{NA}} and \code{NaN}'s
    are removed from \code{x} before the quantiles are computed.}
  \item{names}{logical; if true, the result has a \code{\link{names}}
    attribute.  Set to \code{FALSE} for speedup with many \code{probs}.}
  \item{type}{an integer between 1 and 9 selecting one of the
    nine quantile algorithms detailed below to be used.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  A vector of length \code{length(probs)} is returned;
  if \code{names = TRUE}, it has a \code{\link{names}} attribute.

  \code{\link{NA}} and \code{\link{NaN}} values in \code{probs} are
  propagated to the result.

  The default method works with classed objects sufficiently like
  numeric vectors that \code{sort} and (not needed by types 1 and 3)
  addition of elements and multiplication by a number work correctly.
  Note that as this is in a namespace, the copy of \code{sort} in
  \pkg{base} will be used, not some S4 generic of that name.  Also note
  that that is no check on the \sQuote{correctly}, and so
  e.g.\sspace{}\code{quantile} can be applied to complex vectors which (apart
  from ties) will be ordered on their real parts.

  There is a method for the date-time classes (see
  \code{"\link{POSIXt}"}).  Types 1 and 3 can be used for class
  \code{"\link{Date}"} and for ordered factors.
}
\section{Types}{
  \code{quantile} returns estimates of underlying distribution quantiles
  based on one or two order statistics from the supplied elements in
  \code{x} at probabilities in \code{probs}.  One of the nine quantile
  algorithms discussed in Hyndman and Fan (1996), selected by
  \code{type}, is employed.

  All sample quantiles are defined as weighted averages of
  consecutive order statistics. Sample quantiles of type \eqn{i}
  are defined by:
  \deqn{Q_{i}(p) = (1 - \gamma)x_{j} + \gamma x_{j+1}}{Q[i](p) = (1 - \gamma) x[j] + \gamma x[j+1],}
  where \eqn{1 \le i \le 9},
  \eqn{\frac{j - m}{n} \le p < \frac{j - m + 1}{n}}{(j-m)/n \le p < (j-m+1)/n},
  \eqn{x_{j}}{x[j]} is the \eqn{j}th order statistic, \eqn{n} is the
  sample size, the value of \eqn{\gamma} is a function of
  \eqn{j = \lfloor np + m\rfloor}{j = floor(np + m)} and \eqn{g = np + m - j},
  and \eqn{m} is a constant determined by the sample quantile type.

  \strong{Discontinuous sample quantile types 1, 2, and 3}

  For types 1, 2 and 3, \eqn{Q_i(p)}{Q[i](p)} is a discontinuous
  function of \eqn{p}, with \eqn{m = 0} when \eqn{i = 1} and \eqn{i =
  2}, and \eqn{m = -1/2} when \eqn{i = 3}.

  \describe{
    \item{Type 1}{Inverse of empirical distribution function.
      \eqn{\gamma = 0} if \eqn{g = 0}, and 1 otherwise.}
    \item{Type 2}{Similar to type 1 but with averaging at discontinuities.
      \eqn{\gamma = 0.5} if \eqn{g = 0}, and 1 otherwise.}
    \item{Type 3}{SAS definition: nearest even order statistic.
      \eqn{\gamma = 0} if \eqn{g = 0} and \eqn{j} is even,
      and 1 otherwise.}
  }

  \strong{Continuous sample quantile types 4 through 9}

  For types 4 through 9, \eqn{Q_i(p)}{Q[i](p)} is a continuous function
  of \eqn{p}, with \eqn{\gamma = g}{gamma = g} and \eqn{m} given below. The
  sample quantiles can be obtained equivalently by linear interpolation
  between the points \eqn{(p_k,x_k)}{(p[k],x[k])} where \eqn{x_k}{x[k]}
  is the \eqn{k}th order statistic.  Specific expressions for
  \eqn{p_k}{p[k]} are given below.

  \describe{
    \item{Type 4}{\eqn{m = 0}. \eqn{p_k = \frac{k}{n}}{p[k] = k / n}.
      That is, linear interpolation of the empirical cdf.
    }

    \item{Type 5}{\eqn{m = 1/2}.
      \eqn{p_k = \frac{k - 0.5}{n}}{p[k] = (k - 0.5) / n}.
      That is a piecewise linear function where the knots are the values
      midway through the steps of the empirical cdf.  This is popular
      amongst hydrologists.
    }

    \item{Type 6}{\eqn{m = p}. \eqn{p_k = \frac{k}{n + 1}}{p[k] = k / (n + 1)}.
      Thus \eqn{p_k = \mbox{E}[F(x_{k})]}{p[k] = E[F(x[k])]}.
      This is used by Minitab and by SPSS.
    }

    \item{Type 7}{\eqn{m = 1-p}.
      \eqn{p_k = \frac{k - 1}{n - 1}}{p[k] = (k - 1) / (n - 1)}.
      In this case, \eqn{p_k = \mbox{mode}[F(x_{k})]}{p[k] = mode[F(x[k])]}.
      This is used by S.
    }

    \item{Type 8}{\eqn{m = (p+1)/3}.
      \eqn{p_k = \frac{k - 1/3}{n + 1/3}}{p[k] = (k - 1/3) / (n + 1/3)}.
      Then \eqn{p_k \approx \mbox{median}[F(x_{k})]}{p[k] =~ median[F(x[k])]}.
      The resulting quantile estimates are approximately median-unbiased
      regardless of the distribution of \code{x}.
    }

    \item{Type 9}{\eqn{m = p/4 + 3/8}.
      \eqn{p_k = \frac{k - 3/8}{n + 1/4}}{p[k] = (k - 3/8) / (n + 1/4)}.
      The resulting quantile estimates are approximately unbiased for
      the expected order statistics if \code{x} is normally distributed.
    }
  }
  Further details are provided in Hyndman and Fan (1996) who recommended type 8.
  The default method is type 7, as used by S and by \R < 2.0.0.
}
\author{
  of the version used in \R >= 2.0.0, Ivan Frohne and Rob J Hyndman.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical
  packages, \emph{American Statistician} \bold{50}, 361--365.
}
\seealso{
  \code{\link{ecdf}} for empirical distributions of which
  \code{quantile} is an inverse;
  \code{\link{boxplot.stats}} and \code{\link{fivenum}} for computing
  other versions of quartiles, etc.
}
\examples{
quantile(x <- rnorm(1001)) # Extremes & Quartiles by default
quantile(x,  probs = c(0.1, 0.5, 1, 2, 5, 10, 50, NA)/100)

### Compare different types
p <- c(0.1, 0.5, 1, 2, 5, 10, 50)/100
res <- matrix(as.numeric(NA), 9, 7)
for(type in 1:9) res[type, ] <- y <- quantile(x,  p, type = type)
dimnames(res) <- list(1:9, names(y))
round(res, 3)
}
\keyword{univar}
