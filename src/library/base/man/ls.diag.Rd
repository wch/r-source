\name{ls.diag}
\title{Compute diagnostics for lsfit regression results}
\usage{
ls.diag(ls.out)
}
\alias{ls.diag}
\arguments{
\item{ls.out}{Typically the result of \code{\link{lsfit}(..)}}
}
\description{
Computes basic statistics, including standard errors, \eqn{t-} and \eqn{P-}
values for the regression coefficients and prints them if \code{print.it} is
\code{TRUE}.
}
\value{
A \code{list} with the following numeric components.
\item{std.dev}{The standard deviation of the errors, an estimate of
\eqn{greek(sigma)}}.
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
Belsley, Kuh and Welsch.
}
\seealso{
\code{\link{hat}} for the hat matrix diagonals,
\code{\link{ls.print}},
\code{\link{lm.influence}}, \code{\link{summary.lm}}, \code{\link{anova}}.
}
\examples{
lsD9 <- lsfit(x = as.numeric(gl(2,10,20)), y = weight)
dlsD9 <- ls.diag(lsD9)
plot(dlsD9 $ hat, dlsD9 $ stud.res); abline(h=0, lty=2, col='lightgray')
}
