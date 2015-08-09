% File src/library/stats/man/ls.print.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{ls.print}
\title{Print \code{lsfit} Regression Results}
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
  Usually you would use \code{summary(lm(\dots))} and
  \code{anova(lm(\dots))} to obtain similar output.
}
\seealso{
  \code{\link{ls.diag}}, \code{\link{lsfit}}, also for examples;
  \code{\link{lm}}, \code{\link{lm.influence}} which usually are
  preferable.
}
\keyword{regression}
