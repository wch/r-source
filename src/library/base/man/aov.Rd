\name{aov}
\title{Fit an Analysis of Variance Model}
\usage{
aov(formula, data=sys.parent(), projections=FALSE, contrasts=NULL, ...)
}
\alias{aov}
\alias{print.aov}
\arguments{
 \item{formula}{A formula specifying the model.}
 \item{data}{A data frame in which the variables specified in the
   formula will be found. If missing, the variables are searched for in
   the standard way.}
 \item{projections}{Not implemented: for compatibility with S.}
 \item{contrasts}{A list of contrasts to be used for some of the factors
 in the formula. This is not supported by the current version of
 \code{model.matrix.default} and so has no effect.}
 \item{\dots}{Arguments to be passed to \code{lm}, such as \code{subset}
   or \code{na.action}.}
}
\description{
 This provides a wrapper to \code{\link{lm}} for fitting linear models to
 balanced or unbalanced experimental designs. The call to \code{lm} sets
 \code{singular.ok} to \code{TRUE} to allow for aliased effects.

 The main difference from \code{lm} is in the way \code{\link{print}},
 \code{\link{summary}} and so on handle the fit:  this is expressed in the
 traditional language of the analysis of variance rather than of linear
 models.
}
\value{
  An object of class \code{"lm"} with the additional class \code{"aov"}.
}
\section{WARNING}{
  This implementation is much more basic than that in S; in particular
  \code{Error} is not allowed in the formula and there is no support for
  multistratum models such as split-plot designs.
}
\author{B.D. Ripley}
\seealso{\code{\link{lm}},\code{\link{alias}}}

\examples{
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)
npk.aov <- aov(yield ~ block + N*P*K, npk)
npk.aov
summary(npk.aov)
coefficients(npk.aov)
}
\keyword{models}
\keyword{regression}
