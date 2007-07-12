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
  Computed weighted residuals from a linear model fit.
}
\details{
  Weighted residuals are based on the deviance residuals, which for
  a \code{\link{lm}} fit are the raw residuals \eqn{R_i}{Ri}
  multiplied by \eqn{\sqrt{w_i}}{wi^0.5}, where \eqn{w_i}{wi} are the
  \code{weights} as specified in \code{\link{lm}}'s call.

  Dropping cases with weights zero is compatible with
  \code{\link{influence}} and related functions.
}
\value{
  Numeric vector of length \eqn{n'}, where \eqn{n'} is the number of
  of non-0 weights (\code{drop0 = TRUE}) or the number of
  observations, otherwise.
}
\seealso{\code{\link{residuals}}, \code{\link{lm.influence}}, etc.}
\examples{
utils::example("lm")
all.equal(weighted.residuals(lm.D9),
          residuals(lm.D9))
x <- 1:10
w <- 0:9
y <- rnorm(x)
weighted.residuals(lmxy <- lm(y ~ x, weights = w))
weighted.residuals(lmxy, drop0 = FALSE)
}
\keyword{regression}

