\name{ls.print}
\title{Print `lsfit' Regression Results}
\usage{
ls.print(ls.out, digits = 4, print.it = TRUE)
}
\alias{ls.print}
\arguments{
  \item{ls.out}{Typically the result of \code{\link{lsfit}()}}
  \item{digits}{The number of significant digits used for printing}
  \item{print.it}{a logical indicating whether the result should also be
    printed}
}
\description{
  Computes basic statistics, including standard errors, t- and p-values
  for the regression coefficients and prints them if \code{print.it} is
  \code{TRUE}.
}
\value{
  A list with the components
  \item{summary}{The ANOVA table of the regression}
  \item{coef.table}{matrix with regression coefficients, standard
    errors, t- and p-values}
}
\note{
  Usually, you'd rather use \code{summary(lm(\ldots))} and
  \code{anova(lm(\ldots))} for obtaining similar output.
}
\seealso{
  \code{\link{ls.diag}}, \code{\link{lsfit}}, also for examples;
  \code{\link{lm}}, \code{\link{lm.influence}} which usually are
  preferable.
}
\keyword{regression}
