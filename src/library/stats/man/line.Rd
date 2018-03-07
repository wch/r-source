% File src/library/stats/man/line.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{line}
\alias{line}
\alias{residuals.tukeyline}
\title{Robust Line Fitting}
\description{
  Fit a line robustly as recommended in \emph{Exploratory Data Analysis}.

  Currently the initial median-median line is \emph{not} iterated (as
  opposed to Tukey's \dQuote{resistant line} in the references).
}
\usage{
line(x, y, iter = 1)
}
\arguments{
  \item{x, y}{the arguments can be any way of specifying x-y pairs.  See
    \code{\link{xy.coords}}.}
  \item{iter}{positive integer specifying the number of
    \dQuote{polishing} iterations.  Note that this was hard coded to
    \code{1} in \R versions before 3.5.0, and more importantly that such
    simple iterations may not converge, see Siegel's 9-point example.}
}
\details{
  Cases with missing values are omitted.

  Contrary to the references where the data is split in three (almost)
  equally sized groups with symmetric sizes depending on \eqn{n} and
  \code{n \%\% 3} and computes medians inside each group, the
  \code{line()} code splits into three groups using all observations
  with \code{x[.] <= q1} and \code{x[.] >= q2}, where \code{q1, q2} are
  (a kind of) quantiles for probabilities \eqn{p = 1/3} and \eqn{p = 2/3}
  of the form \code{(x[j1]+x[j2])/2} where \code{j1 = floor(p*(n-1))}
  and \code{j2 = ceiling(p(n-1))}, \code{n = length(x)}.

  Long vectors are not supported yet.
}
\value{
  An object of class \code{"tukeyline"}.

  Methods are available for the generic functions \code{coef},
  \code{residuals}, \code{fitted}, and \code{print}.
}
\references{
  Tukey, J. W. (1977).
  \emph{Exploratory Data Analysis},
  Reading Massachusetts: Addison-Wesley.

  Velleman, P. F. and Hoaglin, D. C. (1981).
  \emph{Applications, Basics and Computing of Exploratory Data Analysis},
  Duxbury Press.
  Chapter 5.
  %% MM has C version of the Fortran code for  rline() there

  Emerson, J. D. and Hoaglin, D. C. (1983).
  Resistant Lines for \eqn{y} versus \eqn{x}.
  Chapter 5 of \emph{Understanding Robust and Exploratory Data Analysis},
  eds. David C. Hoaglin, Frederick Mosteller and John W. Tukey.  Wiley.

  Iain M. Johnstone and Paul F. Velleman (1985).
  The Resistant Line and Related Regression Methods.
  \emph{Journal of the American Statistical Association}, \bold{80},
  1041--1054.
  \doi{10.2307/2288572}.
}
\seealso{
  \code{\link{lm}}.

  There are alternatives for robust linear regression more robust and
  more (statistically) efficient,
  see \code{\link[MASS]{rlm}()} from \CRANpkg{MASS}, or
  \code{\link[robustbase]{lmrob}()}  %% \code{lmrob()}
  from \CRANpkg{robustbase}.
}
\examples{
require(graphics)

plot(cars)
(z <- line(cars))
abline(coef(z))
## Tukey-Anscombe Plot :
plot(residuals(z) ~ fitted(z), main = deparse(z$call))

## Andrew Siegel's pathological 9-point data, y-values multiplied by 3:
d.AS <- data.frame(x = c(-4:3, 12), y = 3*c(rep(0,6), -5, 5, 1))
cAS <- with(d.AS, t(sapply(1:10,
                   function(it) line(x,y, iter=it)$coefficients)))
dimnames(cAS) <- list(paste("it =", format(1:10)), c("intercept", "slope"))
cAS
## iterations started to oscillate, repeating iteration 7,8 indefinitely
}
\keyword{robust}
\keyword{regression}
