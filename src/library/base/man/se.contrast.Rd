% file se.contrast.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{se.contrast}
\title{Standard Errors for Contrasts in Model Terms}
\usage{
se.contrast(object, \dots)
se.contrast.aov(object, contrast.obj,
  coef = contr.helmert(ncol(contrast))[, 1], data = NULL, \dots) 
se.contrast.aovlist(object, contrast.obj,
  coef = contr.helmert(ncol(contrast))[, 1], data = NULL, \dots)
}
\alias{se.contrast}
\alias{se.contrast.aov}
\alias{se.contrast.aovlist}
\arguments{
 \item{object}{A suitable fit, usually from \code{aov}.}
 \item{contrast.obj}{The contrasts for which standard errors are
   requested. This can be specified via a list or via a matrix. A single
   contrast can be specified by a list of logical vectors giving the
   cells to be contrasted. Multiple contrasts should be specified by a
   matrix as returned by \code{contrasts}.}
 \item{coef}{Used when \code{{contrast.obj}} is a list; it should be a
   vector of the same length as the list with zero sum. The default
   value is the first Helmert contrast, which contrasts the first and
   second cell means specified by the list.}
 \item{data}{The data frame used to evaluate \code{contrast.obj}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
 Returns the standard errors for one or more contrasts in an \code{aov}
 object.
}
\details{
 Contrasts are usually used to test if certain means are
 significantly different; it can be easier to use \code{se.contrast}
 than compute directly with the coefficients.

 In multistratum models, the contrasts can appear in more than one
 stratum; the contrast and standard error are computed in the lowest
 stratum and adjusted for efficiencies and comparisons between strata.
}
\value{
  A vector giving the standard errors for each contrast.
}
\author{B.D. Ripley}
\seealso{
  \code{\link{contrasts}},
  \code{\link{model.tables}}
}
\examples{
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)
options(contrasts=c("contr.treatment", "contr.poly"))
npk.aov1 <- aov(yield ~ block + N + K, npk)
se.contrast(npk.aov1, list(N=="0", N=="1"), data=npk)
}
\keyword{models}
