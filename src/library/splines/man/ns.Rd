\name{ns}
\alias{ns}
\title{Generate a Basis Matrix for Natural Cubic Splines}
\description{
  Generate the B-spline basis matrix for a natural cubic spline.
}
\usage{
ns(x, df, knots, intercept = FALSE, Boundary.knots)
}
\arguments{
  \item{x}{the predictor variable.}
  \item{df}{degrees of freedom. One can supply \code{df} rather than
    knots; \code{ns()} then chooses \code{df - 1 - intercept} knots at
    suitably chosen quantiles of \code{x}.}
  \item{knots}{breakpoints that define the spline. The default is no
    knots; together with the natural boundary conditions this results in
    a basis for linear regression on \code{x}.  Typical values are the
    mean or median for one knot, quantiles for more knots. See also
    \code{Boundary.knots}.}
  \item{intercept}{if \code{TRUE}, an intercept is included in the
    basis; default is \code{FALSE}.}
  \item{Boundary.knots}{boundary points at which to impose the natural
    boundary conditions and anchor the B-spline basis (default the range
    of the data).  If both \code{knots} and \code{Boundary.knots} are
    supplied, the basis parameters do not depend on \code{x}. Data can
    extend beyond \code{Boundary.knots}}
}
\value{
    A matrix of dimension \code{length(x) * df} where either \code{df} was
    supplied or if \code{knots} were supplied,
    \code{df = length(knots) + 1 + intercept}.
    Attributes are returned that correspond to the arguments to \code{ns},
    and explicitly give the \code{knots}, \code{Boundary.knots} etc for
    use by \code{predict.ns()}.
    
    \code{ns()} is based on the function \code{spline.des()}.  It
    generates a basis matrix for representing the family of
    piecewise-cubic splines with the specified sequence of
    interior knots, and the natural boundary conditions.  These enforce
    the constraint that the function is linear beyond the boundary knots,
    which can either be supplied, else default to the extremes of the
    data.  A primary use is in modeling formula to directly specify a
    natural spline term in a model.

    Beware of making predictions with new \code{x} values when \code{df}
    is used as an argument. Either use \code{safe.predict.gam()}, or else
    specify \code{knots} and \code{Boundary.knots}.
}
\seealso{
  \code{bs}, \code{poly}, \code{predict.ns}
}
\examples{
data(women)
ns(women$height, df = 5)
summary(fm1 <- lm(weight ~ ns(height, df = 5), data = women))
}
\keyword{smooth}
