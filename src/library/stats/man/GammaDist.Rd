% File src/library/stats/man/GammaDist.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{GammaDist}
\alias{GammaDist}
\alias{dgamma}
\alias{pgamma}
\alias{qgamma}
\alias{rgamma}
\concept{incomplete gamma function}
\title{The Gamma Distribution}
\description{
  Density, distribution function, quantile function and random
  generation for the Gamma distribution with parameters \code{shape} and
  \code{scale}.
}
\usage{
dgamma(x, shape, rate = 1, scale = 1/rate, log = FALSE)
pgamma(q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
qgamma(p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE,
       log.p = FALSE)
rgamma(n, shape, rate = 1, scale = 1/rate)
}
\arguments{
  \item{x, q}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{rate}{an alternative way to specify the scale.}
  \item{shape, scale}{shape and scale parameters.  Must be positive,
    \code{scale} strictly.}
  \item{log, log.p}{logical; if \code{TRUE}, probabilities/densities \eqn{p}
    are returned as \eqn{log(p)}.}
  \item{lower.tail}{logical; if TRUE (default), probabilities are
    \eqn{P[X \le x]}, otherwise, \eqn{P[X > x]}.}
}
\value{
  \code{dgamma} gives the density,
  \code{pgamma} gives the distribution function,
  \code{qgamma} gives the quantile function, and
  \code{rgamma} generates random deviates.

  Invalid arguments will result in return value \code{NaN}, with a warning.
  
  The length of the result is determined by \code{n} for
  \code{rgamma}, and is the maximum of the lengths of the
  numerical parameters for the other functions.  
  
  The numerical parameters other than \code{n} are recycled to the
  length of the result.  Only the first elements of the logical
  parameters are used.
}
\details{
  If \code{scale} is omitted, it assumes the default value of \code{1}.

  The Gamma distribution with parameters \code{shape} \eqn{=\alpha}{= a}
  and \code{scale} \eqn{=\sigma}{= s} has density
  \deqn{
    f(x)= \frac{1}{{\sigma}^{\alpha}\Gamma(\alpha)} {x}^{\alpha-1} e^{-x/\sigma}%
  }{f(x)= 1/(s^a Gamma(a)) x^(a-1) e^-(x/s)}
  for \eqn{x \ge 0}, \eqn{\alpha > 0}{a > 0} and \eqn{\sigma > 0}{s > 0}.
  (Here \eqn{\Gamma(\alpha)}{Gamma(a)} is the function implemented by \R's
  \code{\link{gamma}()} and defined in its help.  Note that \eqn{a = 0}
  corresponds to the trivial distribution with all mass at point 0.)

  The mean and variance are
  \eqn{E(X) = \alpha\sigma}{E(X) = a*s} and
  \eqn{Var(X) = \alpha\sigma^2}{Var(X) = a*s^2}.

  The cumulative hazard \eqn{H(t) = - \log(1 - F(t))}{H(t) = - log(1 - F(t))}
  is
\preformatted{-pgamma(t, ..., lower = FALSE, log = TRUE)
}

  Note that for smallish values of \code{shape} (and moderate
  \code{scale}) a large parts of the mass of the Gamma distribution is
  on values of \eqn{x} so near zero that they will be represented as
  zero in computer arithmetic.  So \code{rgamma} may well return values
  which will be represented as zero.  (This will also happen for very
  large values of \code{scale} since the actual generation is done for
  \code{scale = 1}.)
}
% Have caught all currently known problems; hence no longer say:
%   Similarly, \code{qgamma} has a very hard job for
%   small \code{scale}, and warns of potential unreliability for
%   \code{scale < 1e-10}.
\note{
  The S (Becker \emph{et al} (1988) parametrization was via \code{shape}
  and \code{rate}: S had no \code{scale} parameter.  In \R 2.x.y
  \code{scale} took precedence over \code{rate}, but now it is an error
  to supply both.

  \code{pgamma} is closely related to the incomplete gamma function.  As
  defined by Abramowitz and Stegun 6.5.1 (and by \sQuote{Numerical
    Recipes}) this is
  \deqn{P(a,x) = \frac{1}{\Gamma(a)} \int_0^x t^{a-1} e^{-t} dt}{P(a,x) = 1/Gamma(a) integral_0^x t^(a-1) exp(-t) dt}
  \eqn{P(a, x)} is \code{pgamma(x, a)}.  Other authors (for example
  Karl Pearson in his 1922 tables) omit the normalizing factor,
  defining the incomplete gamma function \eqn{\gamma(a,x)} as
  \eqn{\gamma(a,x) = \int_0^x t^{a-1} e^{-t} dt,}{gamma(a,x) =
    integral_0^x t^(a-1) exp(-t) dt,} i.e., \code{pgamma(x, a) * gamma(a)}.
  Yet other use the \sQuote{upper} incomplete gamma function,
  \deqn{\Gamma(a,x) = \int_x^\infty t^{a-1} e^{-t} dt,}{Gamma(a,x) = integral_x^Inf t^(a-1) exp(-t) dt,}
  which can be computed by
  \code{pgamma(x, a, lower = FALSE) * gamma(a)}.

  Note however that \code{pgamma(x, a, ..)} currently requires \eqn{a > 0},
  whereas the incomplete gamma function is also defined for negative
  \eqn{a}.  In that case, you can use \code{gamma_inc(a,x)} (for
  \eqn{\Gamma(a,x)}) from package \CRANpkg{gsl}.

  See also
  \url{http://en.wikipedia.org/wiki/Incomplete_gamma_function}, or
  \url{http://dlmf.nist.gov/8.2#i}.
}
\source{
  \code{dgamma} is computed via the Poisson density, using code contributed
  by Catherine Loader (see \code{\link{dbinom}}).

  \code{pgamma} uses an unpublished (and not otherwise documented)
  algorithm \sQuote{mainly by Morten Welinder}.

  \code{qgamma} is based on a C translation of

  Best, D. J. and D. E. Roberts (1975).
  Algorithm AS91. Percentage points of the chi-squared distribution.
  \emph{Applied Statistics},  \bold{24}, 385--388.

  plus a final Newton step to improve the approximation.

  \code{rgamma} for \code{shape >= 1} uses

  Ahrens, J. H. and Dieter, U. (1982).
  Generating gamma variates by a modified rejection technique.
  \emph{Communications of the ACM}, \bold{25}, 47--54,

  and for \code{0 < shape < 1} uses

  Ahrens, J. H. and Dieter, U. (1974).
  Computer methods for sampling from gamma, beta, Poisson and binomial
  distributions. \emph{Computing}, \bold{12}, 223--246.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Shea, B. L. (1988)
  Algorithm AS 239,  Chi-squared and incomplete Gamma integral,
  \emph{Applied Statistics (JRSS C)} \bold{37}, 466--473.

  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.
  Chapter 6: Gamma and Related Functions.

  NIST Digital Library of Mathematical Functions.
  \url{http://dlmf.nist.gov/}, section 8.2.
}
\seealso{
  \code{\link{gamma}} for the gamma function.

  \link{Distributions} for other standard distributions, including
  \code{\link{dbeta}} for the Beta distribution and \code{\link{dchisq}}
  for the chi-squared distribution which is a special case of the Gamma
  distribution.
}
\examples{
-log(dgamma(1:4, shape = 1))
p <- (1:9)/10
pgamma(qgamma(p, shape = 2), shape = 2)
1 - 1/exp(qgamma(p, shape = 1))

\donttest{# even for shape = 0.001 about half the mass is on numbers
# that cannot be represented accurately (and most of those as zero)
pgamma(.Machine$double.xmin, 0.001)
pgamma(5e-324, 0.001)  # on most machines 5e-324 is the smallest
                       # representable non-zero number
table(rgamma(1e4, 0.001) == 0)/1e4
}}
\keyword{distribution}
