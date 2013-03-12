% File src/library/splines/man/ns.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{ns}
\alias{ns}
\title{Generate a Basis Matrix for Natural Cubic Splines}
\description{
  Generate the B-spline basis matrix for a natural cubic spline.
}
\usage{
ns(x, df = NULL, knots = NULL, intercept = FALSE,
   Boundary.knots = range(x))
}
\arguments{
  \item{x}{the predictor variable.  Missing values are allowed.}
  \item{df}{degrees of freedom. One can supply \code{df} rather than
    knots; \code{ns()} then chooses \code{df - 1 - intercept} knots at
    suitably chosen quantiles of \code{x} (which will ignore missing
    values).  The default, \code{df = 1}, corresponds to \emph{no} knots.}
  \item{knots}{breakpoints that define the spline.  The default is no
    knots; together with the natural boundary conditions this results in
    a basis for linear regression on \code{x}.  Typical values are the
    mean or median for one knot, quantiles for more knots.  See also
    \code{Boundary.knots}.}
  \item{intercept}{if \code{TRUE}, an intercept is included in the
    basis; default is \code{FALSE}.}
  \item{Boundary.knots}{boundary points at which to impose the natural
    boundary conditions and anchor the B-spline basis (default the range
    of the data).  If both \code{knots} and \code{Boundary.knots} are
    supplied, the basis parameters do not depend on \code{x}. Data can
    extend beyond \code{Boundary.knots}}
}
\details{
  \code{ns} is based on the function \code{\link{spline.des}}.  It
  generates a basis matrix for representing the family of
  piecewise-cubic splines with the specified sequence of
  interior knots, and the natural boundary conditions.  These enforce
  the constraint that the function is linear beyond the boundary knots,
  which can either be supplied or default to the extremes of the
  data.

  A primary use is in modeling formula to directly specify a
  natural spline term in a model: see the examples.
}

\value{
  A matrix of dimension \code{length(x) * df} where either \code{df} was
  supplied or if \code{knots} were supplied,
  \code{df = length(knots) + 1 + intercept}.
  Attributes are returned that correspond to the arguments to \code{ns},
  and explicitly give the \code{knots}, \code{Boundary.knots} etc for
  use by \code{predict.ns()}.
}
\seealso{
  \code{\link{bs}}, \code{\link{predict.ns}}, \code{\link{SafePrediction}}
}
\references{
  Hastie, T. J. (1992)
  Generalized additive models.
  Chapter 7 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
require(stats); require(graphics)
ns(women$height, df = 5)
summary(fm1 <- lm(weight ~ ns(height, df = 5), data = women))

## To see what knots were selected
attr(terms(fm1), "predvars")

## example of safe prediction
plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
ht <- seq(57, 73, length.out = 200)
lines(ht, predict(fm1, data.frame(height = ht)))
\dontshow{
## Consistency:
x <- c(1:3, 5:6)
stopifnot(identical(ns(x), ns(x, df = 1)),
          identical(ns(x, df = 2), ns(x, df = 2, knots = NULL)), # not true till 2.15.2
          !is.null(kk <- attr(ns(x), "knots")), # not true till 1.5.1
          length(kk) == 0)
}
}
\keyword{smooth}
