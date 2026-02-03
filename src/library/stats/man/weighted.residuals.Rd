% File src/library/stats/man/weighted.residuals.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{weighted.residuals}
\alias{weighted.residuals}
\title{Compute Weighted Residuals}
\usage{
weighted.residuals(obj, drop0 = TRUE)
}
\arguments{
  \item{obj}{\R object, typically of class \code{\link{lm}} or
    \code{\link{glm}}.}
  \item{drop0}{logical.  If \code{TRUE}, drop all cases with
    \code{\link{weights} == 0}.}
}
\description{
  Computed weighted residuals from a model fit.
}
\details{
  Weighted residuals are based on the working residuals and working weights, which for
  a \code{\link{lm}} fit are the raw residuals \eqn{R_i}{Ri}
  multiplied by \eqn{\sqrt{w_i}}{wi^0.5}, where \eqn{w_i}{wi} are the
  \code{weights} as specified in \code{\link{lm}}'s call.

  For other models, including \code{\link{glm}} fits, weights and
  residuals are taken from the last iteration of the approximating
  weighted linear model fit. Notice that it is assumed that
  \code{residuals()} and \code{weight()} methods for \code{obj} accept
  \code{type="working"}. 

  Dropping cases with weights zero is compatible with
  \code{\link{influence}} and related functions.
}
\value{
  Numeric vector of length \eqn{n'}, where \eqn{n'} is the number
  of non-0 weights (\code{drop0 = TRUE}) or the number of
  observations, otherwise.
}
\seealso{\code{\link{residuals}}, \code{\link{lm.influence}}, etc.}
\note{Until R version 4.5, this returned deviance residuals, which are
  identical for \code{lm} models, but differs for other models and are
  not well suited for influence measures.}
\examples{
## following on from example(lm)
\dontshow{utils::example("lm", echo = FALSE)}
all.equal(weighted.residuals(lm.D9),
          residuals(lm.D9))
x <- 1:10
w <- 0:9
y <- rnorm(x)
weighted.residuals(lmxy <- lm(y ~ x, weights = w))
weighted.residuals(lmxy, drop0 = FALSE)
}
\keyword{regression}

