% File src/library/splines/man/bs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{bs}
\alias{bs}
\title{B-Spline Basis for Polynomial Splines}
\description{
  Generate the B-spline basis matrix for a polynomial spline.
}
\usage{
bs(x, df = NULL, knots = NULL, degree = 3, intercept = FALSE,
   Boundary.knots = range(x))
}
\arguments{
  \item{x}{the predictor variable.  Missing values are allowed.}
  \item{df}{degrees of freedom; one can specify \code{df} rather than
    \code{knots}; \code{bs()} then chooses \code{df-degree} (minus one
    if there is an intercept) knots at suitable quantiles of \code{x}
    (which will ignore missing values).  The default, \code{NULL},
    corresponds to \emph{no} inner knots, i.e., \code{degree - intercept}.}
  \item{knots}{the \emph{internal} breakpoints that define the
    spline.  The default is \code{NULL}, which results in a basis for
    ordinary polynomial regression.  Typical values are the mean or
    median for one knot, quantiles for more knots.  See also
    \code{Boundary.knots}.}
  \item{degree}{degree of the piecewise polynomial---default is \code{3} for
    cubic splines.}
  \item{intercept}{if \code{TRUE}, an intercept is included in the
    basis; default is \code{FALSE}.}
  \item{Boundary.knots}{boundary points at which to anchor the B-spline
    basis (default the range of the data). If both \code{knots} and
    \code{Boundary.knots} are supplied, the basis parameters do not
    depend on \code{x}. Data can extend beyond \code{Boundary.knots}.}
}
\details{
  \code{bs} is based on the function \code{\link{spline.des}}.
  It generates a basis matrix for
  representing the family of piecewise polynomials with the specified
  interior knots and degree, evaluated at the values of \code{x}.  A
  primary use is in modeling formulas to directly specify a piecewise
  polynomial term in a model.
}
\value{
  A matrix of dimension \code{c(length(x), df)}, where either \code{df}
  was supplied or if \code{knots} were supplied, \code{df =
  length(knots) + degree} plus one if there is an intercept.  Attributes
  are returned that correspond to the arguments to \code{bs}, and
  explicitly give the \code{knots}, \code{Boundary.knots} etc for use by
  \code{predict.bs()}.
}
\seealso{
  \code{\link{ns}}, \code{\link{poly}}, \code{\link{smooth.spline}},
  \code{\link{predict.bs}}, \code{\link{SafePrediction}}
}
\references{
  Hastie, T. J. (1992)
  Generalized additive models.
  Chapter 7 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
require(stats); require(graphics)
bs(women$height, df = 5)
summary(fm1 <- lm(weight ~ bs(height, df = 5), data = women))

## example of safe prediction
plot(women, xlab = "Height (in)", ylab = "Weight (lb)")
ht <- seq(57, 73, length.out = 200)
lines(ht, predict(fm1, data.frame(height = ht)))
\dontshow{
## Consistency:
x <- c(1:3, 5:6)
stopifnot(identical(bs(x), bs(x, df = 3)),
          identical(bs(x, df = 4), bs(x, df = 4, knots = NULL)), # not true till 2.15.2
          !is.null(kk <- attr(bs(x), "knots")), # not true till 1.5.1
          length(kk) == 0)
}}
\keyword{smooth}
