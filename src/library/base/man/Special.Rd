\name{Special}
\alias{Special}
\alias{beta}
\alias{lbeta}
\alias{gamma}
\alias{lgamma}
\alias{digamma}
\alias{trigamma}
\alias{tetragamma}
\alias{pentagamma}
\alias{choose}
\alias{lchoose}
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
digamma(x)
trigamma(x)
tetragamma(x)
pentagamma(x)
choose(n, k)
lchoose(n, k)
}
\arguments{
  \item{a, b, x}{numeric vectors.}
  \item{n, k}{integer vectors.}
}
\details{
%---- This should be improved! ---  want mathematical definitions
%----   AND References to Abramowitz and Stegun
  The functions \code{beta} and \code{lbeta} return the beta function
  and the natural logarithm of the beta function,
  \deqn{B(a,b) = \frac{\Gamma(a)\Gamma(b)}{\Gamma(a+b)}.}{%
        B(a,b) = (Gamma(a)Gamma(b))/(Gamma(a+b)).}

  The functions \code{gamma} and \code{lgamma} return the gamma function
  \eqn{\Gamma(x)} and the natural logarithm of the absolute value of the
  gamma function.

  The functions \code{digamma}, \code{trigamma}, \code{tetragamma} and
  \code{pentagamma} return the first, second, third and fourth
  derivatives of the logarithm of the gamma function.
  \deqn{\code{digamma(x)} = \psi(x) = \frac{d}{dx}\ln\Gamma(x) =
    \frac{\Gamma'(x)}{\Gamma(x)}}{%
    \code{digamma(x)} = psi(x) = d/dx {ln Gamma(x)} = Gamma'(x) / Gamma(x)}

  The functions \code{choose} and \code{lchoose} return binomial
  coefficients and their logarithms.
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
}
\examples{
choose(5, 2)
for (n in 0:10) print(choose(n, k = 0:n))

## gamma has discontinuities are 0, -1, -2, ...
\dontrun{## use plots of points to show this.
curve(gamma(x),-3,4, n=1001, ylim=c(-10,100),
      col="red", lwd=2, main="gamma(x)")
abline(h=0,v=0, lty=3, col="midnightblue")}

x <- seq(.1, 4, length = 201); dx <- diff(x)[1]
par(mfrow = c(2, 3))
for (ch in c("", "l","di","tri","tetra","penta")) {
  is.deriv <- nchar(ch) >= 2
  if (is.deriv) dy <- diff(y) / dx
  nm <- paste(ch, "gamma", sep = "")
  y <- get(nm)(x)
  plot(x, y, type = "l", main = nm, col = "red")
  abline(h = 0, col = "lightgray")
  if (is.deriv) lines(x[-1], dy, col = "blue", lty = 2)
}
}
\keyword{math}
