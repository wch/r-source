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
  \item{x}{numeric vectors whose sample quantiles are wanted.}
  \item{probs}{numeric vector of probabilities with values in \eqn{[0,1]}.}
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
}
\section{Types}{
  \code{quantile} returns estimates of underlying distribution quantiles
  based on one or two order statistics from the supplied elements in
  \code{x} at probabilities in \code{probs}.  One of the nine quantile
  algorithms discussed in Hyndman and Fan (1996), selected by
  \code{type}, is employed.
  
  Sample quantiles of type \eqn{i} are defined by
  \deqn{Q_{i}(p) = (1 - \gamma)x_{j} + \gamma x_{j+1}}{Q[i](p) = (1 - gamma) x[j] + gamma x[j+1],}
  where \eqn{1 \le i \le 9}{1 <= i <= 9},
  \eqn{\frac{j - m}{n} \le p < \frac{j - m + 1}{n}}{(j-m)/n <= p < (j-m+1) n},
  \eqn{x_{j}}{x[j]} is the \eqn{j}th order statistic, \eqn{n} is the
  sample size, and \eqn{m} is a constant determined by the sample
  quantile type. Here \eqn{\gamma}{gamma} depends on the fractional part
  of \eqn{g = np+m-j}.

  For the continuous sample quantile types (4 through 9), the sample
  quantiles can be obtained by linear interpolation between the \eqn{k}th
  order statistic and \eqn{p(k)}: \deqn{p(k) = \frac{k - \alpha} {n -
      \alpha - \beta + 1}}{p(k) = (k - alpha) / (n - alpha - beta + 1),} where
  \eqn{\alpha}{alpha} and \eqn{\beta}{beta} are constants determined by
  the type.  Further, \eqn{m = \alpha + p \left( 1 - \alpha - \beta
    \right)}{m = alpha + p(1 - alpha - beta)}, and \eqn{\gamma = g}{gamma = g}.

  \strong{Discontinuous sample quantile types 1, 2, and 3}

  \describe{
    \item{Type 1}{Inverse of empirical distribution function.}
    \item{Type 2}{Similar to type 1 but with averaging at discontinuities.}
    \item{Type 3}{SAS definition: nearest even order statistic.}
  }


  \strong{Continuous sample quantile types 4 through 9}

  \describe{
    \item{Type 4}{\eqn{p(k) = \frac{k}{n}}{p(k) = k / n}.
      That is, linear interpolation of the empirical cdf.
    }
    
    \item{Type 5}{\eqn{p(k) = \frac{k - 0.5}{n}}{p(k) = (k - 0.5) / n}.
      That is a piecewise linear function where the knots are the values
      midway through the steps of the empirical cdf. This is popular
      amongst hydrologists.
    }
    
    \item{Type 6}{\eqn{p(k) = \frac{k}{n + 1}}{p(k) = k / (n + 1)}.
      Thus \eqn{p(k) = \mbox{E}[F(x_{k})]}{p(k) = E[F(x[k])]}.
      This is used by Minitab and by SPSS.
    }
    
    \item{Type 7}{\eqn{p(k) = \frac{k - 1}{n - 1}}{p(k) = (k - 1) / (n - 1)}.
      In this case, \eqn{p(k) = \mbox{mode}[F(x_{k})]}{p(k) = mode[F(x[k])]}.
      This is used by S.
    }
    
    \item{Type 8}{\eqn{p(k) = \frac{k - \frac{1}{3}}{n + \frac{1}{3}}}{p(k) = (k - 1/3) / (n + 1/3)}.
      Then \eqn{p(k) \approx \mbox{median}[F(x_{k})]}{p(k) =~ median[F(x[k])]}.
      The resulting quantile estimates are approximately median-unbiased
      regardless of the distribution of \code{x}.
    }

    \item{Type 9}{\eqn{p(k) = \frac{k - \frac{3}{8}}{n + \frac{1}{4}}}{p(k) = (k - 3/8) / (n + 1/4)}.
      The resulting quantile estimates are approximately unbiased if
      \code{x} is normally distributed.
    }
  }    
  Hyndman and Fan (1996) recommend type 8.
  The default method is type 7, as used by S and by \R < 2.0.0.
}
\author{
  of the version of \R 2.0.0, Ivan Frohne and Rob J Hyndman.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Hyndman, R. J. and Fan, Y. (1996) Sample quantiles in statistical
  packages, \emph{American Statistician}, \bold{50}, 361--365.
}
\seealso{
  \code{\link{ecdf}} for empirical distributions of which
  \code{quantile} is the \dQuote{inverse};
  \code{\link{boxplot.stats}} and \code{\link{fivenum}} for computing
  \dQuote{versions} of quartiles, etc.
}
\examples{
quantile(x <- rnorm(1001))# Extremes & Quartiles by default
quantile(x,  probs=c(.1,.5,1,2,5,10,50, NA)/100)

### Compare different types
p <- c(0.1,0.5,1,2,5,10,50)/100
res <- matrix(as.numeric(NA), 9, 7)
for(type in 1:9) res[type, ] <- y <- quantile(x,  p, type=type)
dimnames(res) <- list(1:9, names(y))
round(res, 3)
}
\keyword{univar}
