% File src/library/stats/man/coef.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{coef}
\title{Extract Model Coefficients}
\alias{coef}
\alias{coefficients}
\alias{coef.default}% not exported
\alias{coef.aov}
\usage{
coef(object, \dots)
coefficients(object, \dots)
\S3method{coef}{default}(object, complete = TRUE, \dots)
\S3method{coef}{aov}(object, complete = FALSE, \dots)
}
\arguments{
  \item{object}{an object for which the extraction of model coefficients is
    meaningful.}
  \item{complete}{for the default (used for \code{lm}, etc) and
    \code{aov} methods:
    logical indicating if the full coefficient vector should be returned
    also in case of an over-determined system where some coefficients
    will be set to \code{\link{NA}}, see also \code{\link{alias}}.  Note
    that the default \emph{differs} for \code{\link{lm}()} and
    \code{\link{aov}()} results.}
  \item{\dots}{other arguments.}
}
\description{
  \code{coef} is a generic function which extracts model coefficients
  from objects returned by modeling functions.  \code{coefficients} is
  an \emph{alias} for it.
}
\details{
  All object classes which are returned by model fitting functions
  should provide a \code{coef} method or use the default one.
  (Note that the method is for \code{coef} and not \code{coefficients}.)

  The \code{"aov"} method does not report aliased coefficients (see
  \code{\link{alias}}) by default where \code{complete = FALSE}.

  The \code{complete} argument also exists for compatibility with
  \code{\link{vcov}} methods, and \code{coef} and \code{aov} methods for
  other classes should typically also keep the \code{complete = *}
  behavior in sync.  By that, with \code{p <- length(coef(obj, complete = TF))},
  \code{dim(vcov(obj, complete = TF)) == c(p,p)} will be fulfilled for both
  \code{complete} settings and the default.
}
\value{
  Coefficients extracted from the model object \code{object}.

  For standard model fitting classes this will be a named numeric vector.
  For \code{"maov"} objects (produced by \code{\link{aov}}) it will be a matrix.
}
\seealso{
  \code{\link{fitted.values}} and \code{\link{residuals}} for related methods;
  \code{\link{glm}}, \code{\link{lm}} for model fitting.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth & Brooks/Cole.
}
\examples{
x <- 1:5; coef(lm(c(1:3, 7, 6) ~ x))
}
\keyword{regression}
\keyword{models}
