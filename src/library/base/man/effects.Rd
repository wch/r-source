\name{effects}
\title{Effects from Fitted Model}
\usage{
effects(object, \dots)
effects.lm(object, set.sign=FALSE)
}
\alias{effects}
\alias{effects.lm}
\alias{effects.glm}
\arguments{
 \item{object}{list; typically, the result of a model fitting function
   such as \code{\link{lm}}}
 \item{set.sign}{logical ~~Describe \code{set.sign} here~~ }
}
\description{
  Returns (orthogonal) effects from a fitted (generalized) regression
  model.  In case of a full rank \eqn{n \times p}{n * p}
  \code{\link{model.matrix}}, the first \eqn{p} effects correspond to
  coefficients and the remaining \eqn{n-p} ones to residuals.
}
\details{

  \dots\dots FIXME \dots\dots

  This page is not complete yet!
}
\value{
  A (named) numeric vector of the same length as
  \code{\link{residuals}}.  The first \eqn{p} values are labeled
  according to the corresponding coefficients, the remaining ones are
  unlabeled.

  The result currently has \code{\link{class}} \code{"coef"}.

  \dots\dots FIXME \dots\dots
}
\seealso{\code{\link{coef}}, \code{\link{fitted}}, etc.}
\examples{
y <- c(1:3,7,5)
x <- c(1:3,6:7)
( ee <- effects(lm(y ~ x)) )
c(round(ee - effects(lm(y+10 ~ I(x-3.8))),3))# just the first is different
}
\keyword{models}
\keyword{regression}
