% File src/library/stats/man/anova.lm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{anova.lm}
\alias{anova.lm}
\alias{anova.lmlist}
\title{ANOVA for Linear Model Fits}
\usage{
\method{anova}{lm}(object, \dots)

\method{anova}{lmlist}(object, \dots, scale = 0, test = "F")
}
\description{
  Compute an analysis of variance table for one or more linear model fits.
}
\arguments{
  \item{object, \dots}{objects of class \code{lm}, usually, a result of a
    call to \code{\link{lm}}.}
  \item{test}{a character string specifying the test statistic to be
    used. Can be one of \code{"F"}, \code{"Chisq"} or \code{"Cp"},
    with partial matching allowed, or \code{NULL} for no test.}
  \item{scale}{numeric. An estimate of the noise variance
    \eqn{\sigma^2}. If zero this will be estimated from the
    largest model considered.
  }
}
\details{
  Specifying a single object gives a sequential analysis of variance
  table for that fit.  That is, the reductions in the residual sum of
  squares as each term of the formula is added in turn are given in as
  the rows of a table, plus the residual sum of squares.

  The table will contain F statistics (and P values) comparing the
  mean square for the row to the residual mean square.

  If more than one object is specified, the table has a row for the
  residual degrees of freedom and sum of squares for each model.
  For all but the first model, the change in degrees of freedom and sum
  of squares is also given. (This only make statistical sense if the
  models are nested.)  It is conventional to list the models from
  smallest to largest, but this is up to the user.

  Optionally the table can include test statistics.  Normally the
  F statistic is most appropriate, which compares the mean square for a
  row to the residual sum of squares for the largest model considered.
  If \code{scale} is specified chi-squared tests can be used. Mallows'
  \eqn{C_p}{Cp} statistic is the residual sum of squares plus twice the
  estimate of \eqn{\sigma^2}{sigma^2} times the residual degrees of freedom.
}
\value{
  An object of class \code{"anova"} inheriting from class \code{"data.frame"}.
}
\section{Warning}{
  The comparison between two or more models will only be valid if they
  are fitted to the same dataset. This may be a problem if there are
  missing values and \R's default of \code{na.action = na.omit} is used,
  and \code{anova.lmlist} will detect this with an error.
}
\references{
  Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{anova}}.

  \code{\link{drop1}} for
  so-called \sQuote{type II} anova where each term is dropped one at a
  time respecting their hierarchy.
}
\examples{
## sequential table
fit <- lm(sr ~ ., data = LifeCycleSavings)
anova(fit)

## same effect via separate models
fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
fit1 <- update(fit0, . ~ . + pop15)
fit2 <- update(fit1, . ~ . + pop75)
fit3 <- update(fit2, . ~ . + dpi)
fit4 <- update(fit3, . ~ . + ddpi)
anova(fit0, fit1, fit2, fit3, fit4, test = "F")

anova(fit4, fit2, fit0, test = "F") # unconventional order
}
\keyword{regression}
\keyword{models}
