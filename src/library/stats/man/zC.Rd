% File src/library/stats/man/zC.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{C}
\title{Sets Contrasts for a Factor}
\usage{
C(object, contr, how.many, \dots)
}
\alias{C}
\arguments{
  \item{object}{a factor or ordered factor}
  \item{contr}{which contrasts to use. Can be a matrix with one row for
    each level of the factor or a suitable function like
    \code{contr.poly} or a character string giving the name of the function}
  \item{how.many}{the number of contrasts to set, by default one less
    than \code{nlevels(object)}.}
  \item{\dots}{additional arguments for the function \code{contr}.}
}
\description{
  Sets the \code{"contrasts"} attribute for the factor.
}
\details{
  For compatibility with S, \code{contr} can be \code{treatment},
  \code{helmert}, \code{sum} or \code{poly} (without quotes) as shorthand
  for \code{contr.treatment} and so on.
}
\value{
  The factor \code{object} with the \code{"contrasts"} attribute set.
}
\seealso{
  \code{\link{contrasts}}, \code{\link{contr.sum}}, etc.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical models.}
  Chapter 2 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
## reset contrasts to defaults
options(contrasts = c("contr.treatment", "contr.poly"))
tens <- with(warpbreaks, C(tension, poly, 1))
\donttest{attributes(tens)}
## tension SHOULD be an ordered factor, but as it is not we can use
aov(breaks ~ wool + tens + tension, data = warpbreaks)

## show the use of ...  The default contrast is contr.treatment here
summary(lm(breaks ~ wool + C(tension, base = 2), data = warpbreaks))


# following on from help(esoph)
model3 <- glm(cbind(ncases, ncontrols) ~ agegp + C(tobgp, , 1) +
     C(alcgp, , 1), data = esoph, family = binomial())
summary(model3)
}
\keyword{models}
