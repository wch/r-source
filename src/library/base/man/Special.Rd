\name{Special}
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
\title{Special Functions of Mathematics}
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
  \item{a, b, x, n}{numeric vectors.}
  \item{k, deriv}{integer vectors.}
}
\details{
  The functions \code{beta} and \code{lbeta} return the beta function
  and the natural logarithm of the beta function,
  \deqn{B(a,b) = \frac{\Gamma(a)\Gamma(b)}{\Gamma(a+b)}.}{%
    B(a,b) = (Gamma(a)Gamma(b))/(Gamma(a+b)).}
  The formal definition is
  \deqn{B(a, b) = \int_0^1 t^{a-1} (1-t)^{b-1} dt}{integral_0^1 t^(a-1) (1-t)^(b-1) dt}
  (Abramowitz and Stegun (6.2.1), page 258).

  The functions \code{gamma} and \code{lgamma} return the gamma function
  \eqn{\Gamma(x)} and the natural logarithm of the absolute value of the
  gamma function.  The gamma function is defined by
  (Abramowitz and Stegun (6.1.1), page 255)
  \deqn{\Gamma(x) = \int_0^\infty t^{a-1} e^{-t} dt}{integral_0^Inf t^(a-1) exp(-t) dt}
  \code{factorial(x)} is \eqn{x!} and identical to
  \code{gamma(x+1)} and \code{lfactorial} is \code{lgamma(x+1)}.

  The functions \code{digamma} and \code{trigamma} return the first and second
  derivatives of the logarithm of the gamma function.
  \code{psigamma(x, deriv)} (\code{deriv >= 0}) is more generally
  computing the \code{deriv}-th derivative of \eqn{\psi(x)}.
  \deqn{\code{digamma(x)} = \psi(x) = \frac{d}{dx}\ln\Gamma(x) =
    \frac{\Gamma'(x)}{\Gamma(x)}}{%
    \code{digamma(x)} = psi(x) = d/dx {ln Gamma(x)} = Gamma'(x) / Gamma(x)}

  The functions \code{choose} and \code{lchoose} return binomial
  coefficients and their logarithms.  Note that \code{choose(n,k)} is
  defined for all real numbers \eqn{n} and integer \eqn{k}.  For \eqn{k
    \ge 1}{k >= 1} as \eqn{n(n-1)\cdots(n-k+1) / k!}{n(n-1)...(n-k+1) / k!},
  as \eqn{1} for \eqn{k = 0} and as \eqn{0} for negative \eqn{k}.
  \cr \code{choose(*,k)} uses direct arithmetic (instead of
  \code{[l]gamma} calls) for small \code{k}, for speed and accuracy reasons.

  The \code{gamma}, \code{lgamma}, \code{digamma} and \code{trigamma}
  functions are generic: methods can be defined for them individually or
  via the \code{\link{Math}} group generic.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (for \code{gamma} and \code{lgamma}.)

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
choose(5, 2)
for (n in 0:10) print(choose(n, k = 0:n))

factorial(100)
lfactorial(10000)

## gamma has 1st order poles at 0, -1, -2, ...
x <- sort(c(seq(-3,4, length=201), outer(0:-3, (-1:1)*1e-6, "+")))
plot(x, gamma(x), ylim=c(-20,20), col="red", type="l", lwd=2,
     main=expression(Gamma(x)))
abline(h=0, v=-3:0, lty=3, col="midnightblue")

x <- seq(.1, 4, length = 201); dx <- diff(x)[1]
par(mfrow = c(2, 3))
for (ch in c("", "l","di","tri","tetra","penta")) {
  is.deriv <- nchar(ch) >= 2
  nm <- paste(ch, "gamma", sep = "")
  if (is.deriv) {
    dy <- diff(y) / dx # finite difference
    der <- which(ch == c("di","tri","tetra","penta")) - 1
    nm2 <- paste("psigamma(*, deriv = ", der,")",sep='')
    nm  <- if(der >= 2) nm2 else paste(nm, nm2, sep = " ==\n")
    y <- psigamma(x, deriv=der)
  } else {
    y <- get(nm)(x)
  }
  plot(x, y, type = "l", main = nm, col = "red")
  abline(h = 0, col = "lightgray")
  if (is.deriv) lines(x[-1], dy, col = "blue", lty = 2)
}

## "Extended" Pascal triangle:
fN <- function(n) formatC(n, wid=2)
for (n in -4:10) cat(fN(n),":", fN(choose(n, k= -2:max(3,n+2))), "\n")

## R code version of choose()  [simplistic; warning for k < 0]:
mychoose <- function(r,k)
    ifelse(k <= 0, (k==0),
           sapply(k, function(k) prod(r:(r-k+1))) / factorial(k))
k <- -1:6
cbind(k=k, choose(1/2, k), mychoose(1/2, k))

## Binomial theorem for n=1/2 ;
## sqrt(1+x) = (1+x)^(1/2) = sum_{k=0}^Inf  choose(1/2, k) * x^k :
k <- 0:10 # 10 is sufficient for ~ 9 digit precision:
sqrt(1.25)
sum(choose(1/2, k)* .25^k)

\dontshow{
stopifnot(all.equal(  choose(1/2, -1:9),
                    mychoose(1/2, -1:9)),
          all.equal(sqrt(1.25),
                    sum(choose(1/2, k)* .25^k)))
}
}
\keyword{math}
