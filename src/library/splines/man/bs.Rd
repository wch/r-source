\name{bs}
\title{Generate a Basis for Polynomial Splines}
\usage{
bs(x, df, knots, degree=3, intercept=FALSE, Boundary.knots)
}
\alias{bs}
\arguments{
  \item{x}{the predictor variable.}
  \item{df}{degrees of freedom; one can specify \code{df} rather than
    \code{knots}; \code{bs()} then chooses \code{df-degree-1} knots at
    suitable quantiles of \code{x}.} 
  \item{knots}{the \emph{internal} breakpoints that define the
    spline.  The default is \code{NULL}, which results in a basis for
    ordinary polynomial regression. Typical values are the mean or
    median for one knot, quantiles for more knots. See also
    \code{Boundary.knots}.}
  \item{degree}{degree of the piecewise polynomial---default is 3 for
    cubic splines.}
  \item{intercept}{if \code{TRUE}, an intercept is included in the
    basis; default is \code{FALSE}.}
  \item{Boundary.knots}{boundary points at which to anchor the B-spline
    basis (default the range of the data). If both \code{knots} and
    \code{Boundary.knots} are supplied, the basis parameters do not
    depend on \code{x}. Data can extend beyond \code{Boundary.knots}} 
}
\description{
  Generate the B-spline basis matrix for a cubic spline.
}
\value{
  a matrix of dimension \code{length(x) * df}, where either \code{df}
  was supplied or if \code{knots} were supplied,
  \code{df = length(knots) + 3 + intercept}.  Attributes are returned
  that  correspond to the arguments to \code{bs}, and explicitly give
  the \code{knots}, \code{Boundary.knots} etc for use by
  \code{predict.bs()}.

  \code{bs()} is based on the function \code{spline.des()} written by
  Douglas Bates. It generates a basis matrix for representing the family
  of piecewise polynomials with the specified interior knots and degree,
  evaluated at the values of \code{x}. A primary use is in modeling
  formulas to directly specify a piecewise polynomial term in a model. 

  Beware of making predictions with new \code{x} values when \code{df}
  is used as an argument. Either use \code{safe.predict.gam()}, or else
  specify \code{knots} and \code{Boundary.knots}.
}
\seealso{
  \code{ns}, \code{poly}, \code{smooth.spline},
  \code{predict.bs}.
}
\examples{
library(splines)
data(women)
bs(women$height, df = 5)
summary(fm1 <- lm(weight ~ bs(height, df = 5), data = women))
}


