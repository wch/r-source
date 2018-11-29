% File src/library/stats/man/ls.diag.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{ls.diag}
\title{Compute Diagnostics for \code{lsfit} Regression Results}
\usage{
ls.diag(ls.out)
}
\alias{ls.diag}
\arguments{
  \item{ls.out}{Typically the result of \code{\link{lsfit}()}}
}
\description{
  Computes basic statistics, including standard errors, t- and p-values
  for the regression coefficients.
}
\value{
  A \code{list} with the following numeric components.
  \item{std.dev}{The standard deviation of the errors, an estimate of
    \eqn{\sigma}.}
  \item{hat}{diagonal entries \eqn{h_{ii}} of the hat matrix \eqn{H}}
  \item{std.res}{standardized residuals}
  \item{stud.res}{studentized residuals}
  \item{cooks}{Cook's distances}
  \item{dfits}{DFITS statistics}
  \item{correlation}{correlation matrix}
  \item{std.err}{standard errors of the regression coefficients}
  \item{cov.scaled}{Scaled covariance matrix of the coefficients}
  \item{cov.unscaled}{Unscaled covariance matrix of the coefficients}
}
\references{
  Belsley, D. A., Kuh, E. and Welsch, R. E. (1980)
  \emph{Regression Diagnostics.}
  New York: Wiley.
}
\seealso{
  \code{\link{hat}} for the hat matrix diagonals,
  \code{\link{ls.print}},
  \code{\link{lm.influence}}, \code{\link{summary.lm}},
  \code{\link{anova}}.
}
\examples{
\dontshow{utils::example("lm", echo = FALSE)}
##-- Using the same data as the lm(.) example:
lsD9 <- lsfit(x = as.numeric(gl(2, 10, 20)), y = weight)
dlsD9 <- ls.diag(lsD9)
\donttest{utils::str(dlsD9, give.attr = FALSE)}
abs(1 - sum(dlsD9$hat) / 2) < 10*.Machine$double.eps # sum(h.ii) = p
plot(dlsD9$hat, dlsD9$stud.res, xlim = c(0, 0.11))
abline(h = 0, lty = 2, col = "lightgray")
}
\keyword{regression}
