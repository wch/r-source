% File src/library/base/man/Special.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{Special}
\title{Special Functions of Mathematics}
\alias{Special}
\alias{beta}
\alias{lbeta}
\alias{gamma}
\alias{lgamma}
\alias{psigamma}
\alias{digamma}
\alias{trigamma}
\alias{choose}
\alias{lchoose}
\alias{factorial}
\alias{lfactorial}
\concept{tetragamma}% existed till 1.9.0
\concept{pentagamma}
\concept{polygamma}
\concept{binomial coefficient}
\concept{psi function}
\description{
  Special mathematical functions related to the beta and gamma
  functions.
}
\usage{
beta(a, b)
lbeta(a, b)

gamma(x)
lgamma(x)
psigamma(x, deriv = 0)
digamma(x)
trigamma(x)

choose(n, k)
lchoose(n, k)
factorial(x)
lfactorial(x)
}
\arguments{
  \item{a, b}{non-negative numeric vectors.}
  \item{x, n}{numeric vectors.}
  \item{k, deriv}{integer vectors.}
}
\details{
  The functions \code{beta} and \code{lbeta} return the beta function
  and the natural logarithm of the beta function,
  \deqn{B(a,b) = \frac{\Gamma(a)\Gamma(b)}{\Gamma(a+b)}.}{B(a,b) = \Gamma(a)\Gamma(b)/\Gamma(a+b).}
  The formal definition is
  \deqn{B(a, b) = \int_0^1 t^{a-1} (1-t)^{b-1} dt}{integral_0^1 t^(a-1) (1-t)^(b-1) dt}
  (Abramowitz and Stegun section 6.2.1, page 258).  Note that it is only
  defined in \R for non-negative \code{a} and \code{b}, and is infinite
  if either is zero.

  The functions \code{gamma} and \code{lgamma} return the gamma function
  \eqn{\Gamma(x)} and the natural logarithm of \emph{the absolute value of} the
  gamma function.  The gamma function is defined by
  (Abramowitz and Stegun section 6.1.1, page 255)
  \deqn{\Gamma(x) = \int_0^\infty t^{x-1} e^{-t} dt}{\Gamma(x) = integral_0^Inf t^(x-1) exp(-t) dt}
  for all real \code{x} except zero and negative integers (when
  \code{NaN} is returned).  There will be a warning on possible loss of
  precision for values which are too close (within about
  \eqn{10^{-8}}{1e-8})) to a negative integer less than \samp{-10}.

  \code{factorial(x)} (\eqn{x!} for non-negative integer \code{x})
  is defined to be \code{gamma(x+1)} and \code{lfactorial} to be
  \code{lgamma(x+1)}.

  The functions \code{digamma} and \code{trigamma} return the first and second
  derivatives of the logarithm of the gamma function.
  \code{psigamma(x, deriv)} (\code{deriv >= 0}) computes the
  \code{deriv}-th derivative of \eqn{\psi(x)}.
  \deqn{\code{digamma(x)} = \psi(x) = \frac{d}{dx}\ln\Gamma(x) =
    \frac{\Gamma'(x)}{\Gamma(x)}}{digamma(x) = \psi(x) = d/dx{ln \Gamma(x)} = \Gamma'(x) / \Gamma(x)}
  This is often called the \sQuote{polygamma} function, e.g. in
  Abramowitz and Stegun (section 6.4.1, page 260); and higher
  derivatives (\code{deriv = 2:4}) have occasionally been called
  \sQuote{tetragamma}, \sQuote{pentagamma}, and \sQuote{hexagamma}.

  The functions \code{choose} and \code{lchoose} return binomial
  coefficients and the logarithms of their absolute values.  Note that
  \code{choose(n, k)} is defined for all real numbers \eqn{n} and integer
  \eqn{k}.  For \eqn{k \ge 1} it is defined as
  \eqn{n(n-1)\cdots(n-k+1) / k!}{n(n-1)\dots(n-k+1) / k!},
  as \eqn{1} for \eqn{k = 0} and as \eqn{0} for negative \eqn{k}.
  Non-integer values of \code{k} are rounded to an integer, with a warning.
  \cr \code{choose(*, k)} uses direct arithmetic (instead of
  \code{[l]gamma} calls) for small \code{k}, for speed and accuracy
  reasons.  Note the function \code{\link[utils]{combn}} (package
  \pkg{utils}) for enumeration of all possible combinations.

  The \code{gamma}, \code{lgamma}, \code{digamma} and \code{trigamma}
  functions are \link{internal generic} \link{primitive} functions: methods can be
  defined for them individually or via the
  \code{\link[=S3groupGeneric]{Math}} group generic.
}
\source{
  \code{gamma}, \code{lgamma}, \code{beta} and \code{lbeta} are based on
  C translations of Fortran subroutines by W. Fullerton of Los Alamos
  Scientific Laboratory (now available as part of SLATEC).

  \code{digamma}, \code{trigamma} and \code{psigamma} are based on

  Amos, D. E. (1983). A portable Fortran subroutine for
  derivatives of the psi function, Algorithm 610,
  \emph{ACM Transactions on Mathematical Software} \bold{9(4)}, 494--502.

}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole. (For \code{gamma} and \code{lgamma}.)

  Abramowitz, M. and Stegun, I. A. (1972)
  \emph{Handbook of Mathematical Functions.} New York: Dover.
  Chapter 6: Gamma and Related Functions.
}
\seealso{
  \code{\link{Arithmetic}} for simple, \code{\link{sqrt}} for
  miscellaneous mathematical functions and \code{\link{Bessel}} for the
  real Bessel functions.

  For the incomplete gamma function see \code{\link{pgamma}}.
}
\examples{
require(graphics)

choose(5, 2)
for (n in 0:10) print(choose(n, k = 0:n))

factorial(100)
lfactorial(10000)

## gamma has 1st order poles at 0, -1, -2, ...
## this will generate loss of precision warnings, so turn off
op <- options("warn")
options(warn = -1)
x <- sort(c(seq(-3, 4, length.out = 201), outer(0:-3, (-1:1)*1e-6, "+")))
plot(x, gamma(x), ylim = c(-20,20), col = "red", type = "l", lwd = 2,
     main = expression(Gamma(x)))
abline(h = 0, v = -3:0, lty = 3, col = "midnightblue")
options(op)

x <- seq(0.1, 4, length.out = 201); dx <- diff(x)[1]
par(mfrow = c(2, 3))
for (ch in c("", "l","di","tri","tetra","penta")) {
  is.deriv <- nchar(ch) >= 2
  nm <- paste0(ch, "gamma")
  if (is.deriv) {
    dy <- diff(y) / dx # finite difference
    der <- which(ch == c("di","tri","tetra","penta")) - 1
    nm2 <- paste0("psigamma(*, deriv = ", der,")")
    nm  <- if(der >= 2) nm2 else paste(nm, nm2, sep = " ==\n")
    y <- psigamma(x, deriv = der)
  } else {
    y <- get(nm)(x)
  }
  plot(x, y, type = "l", main = nm, col = "red")
  abline(h = 0, col = "lightgray")
  if (is.deriv) lines(x[-1], dy, col = "blue", lty = 2)
}
par(mfrow = c(1, 1))

## "Extended" Pascal triangle:
fN <- function(n) formatC(n, width=2)
for (n in -4:10) {
    cat(fN(n),":", fN(choose(n, k = -2:max(3, n+2))))
    cat("\n")
}

## R code version of choose()  [simplistic; warning for k < 0]:
mychoose <- function(r, k)
    ifelse(k <= 0, (k == 0),
           sapply(k, function(k) prod(r:(r-k+1))) / factorial(k))
k <- -1:6
cbind(k = k, choose(1/2, k), mychoose(1/2, k))

## Binomial theorem for n = 1/2 ;
## sqrt(1+x) = (1+x)^(1/2) = sum_{k=0}^Inf  choose(1/2, k) * x^k :
k <- 0:10 # 10 is sufficient for ~ 9 digit precision:
sqrt(1.25)
sum(choose(1/2, k)* .25^k)

\dontshow{
k. <- 1:9
stopifnot(all.equal( (choose(1/2, k.) -> ck.),
                    mychoose(1/2, k.)),
          all.equal(lchoose(1/2, k.), log(abs(ck.))),
          all.equal(sqrt(1.25),
                    sum(choose(1/2, k)* .25^k)))
}
}
\keyword{math}
