% File src/library/stats/man/coef.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{coef}
\title{Extract Model Coefficients}
\usage{
coef(object, \dots)
coefficients(object, \dots)
}
\alias{coef}
\alias{coefficients}
\arguments{
  \item{object}{an object for which the extraction of model coefficients is
    meaningful.}
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

  Class \code{"aov"} has a \code{coef} method that does not report
  aliased coefficients (see \code{\link{alias}}).
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
