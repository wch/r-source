% File src/library/stats/man/se.contrast.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{se.contrast}
\alias{se.contrast}
\alias{se.contrast.aov}
\alias{se.contrast.aovlist}
\title{Standard Errors for Contrasts in Model Terms}
\usage{
se.contrast(object, \dots)
\method{se.contrast}{aov}(object, contrast.obj,
           coef = contr.helmert(ncol(contrast))[, 1],
           data = NULL, \dots)
}
\arguments{
 \item{object}{A suitable fit, usually from \code{aov}.}
 \item{contrast.obj}{The contrasts for which standard errors are
   requested.  This can be specified via a list or via a matrix.  A
   single contrast can be specified by a list of logical vectors giving
   the cells to be contrasted.  Multiple contrasts should be specified
   by a matrix, each column of which is a numerical contrast vector
   (summing to zero).
   }
 \item{coef}{used when \code{contrast.obj} is a list; it should be a
   vector of the same length as the list with zero sum.  The default
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
  than compute them directly from the coefficients.

  In multistratum models, the contrasts can appear in more than one
  stratum, in which case the standard errors are computed in the lowest
  stratum and adjusted for efficiencies and comparisons between
  strata. (See the comments in the note in the help for
  \code{\link{aov}} about using orthogonal contrasts.)  Such standard
  errors are often conservative.

  Suitable matrices for use with \code{coef} can be found by
  calling \code{\link{contrasts}} and indexing the columns by a factor.
}
\value{
  A vector giving the standard errors for each contrast.
}
\seealso{
  \code{\link{contrasts}}, \code{\link{model.tables}}
}
\examples{
## From Venables and Ripley (2002) p.165.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block = gl(6,4), N = factor(N), P = factor(P),
                  K = factor(K), yield = yield)
## Set suitable contrasts.
options(contrasts = c("contr.helmert", "contr.poly"))
npk.aov1 <- aov(yield ~ block + N + K, data = npk)
se.contrast(npk.aov1, list(N == "0", N == "1"), data = npk)
# or via a matrix
cont <- matrix(c(-1,1), 2, 1, dimnames = list(NULL, "N"))
se.contrast(npk.aov1, cont[N, , drop = FALSE]/12, data = npk)

## test a multi-stratum model
npk.aov2 <- aov(yield ~ N + K + Error(block/(N + K)), data = npk)
se.contrast(npk.aov2, list(N == "0", N == "1"))


## an example looking at an interaction contrast
## Dataset from R.E. Kirk (1995)
## 'Experimental Design: procedures for the behavioral sciences'
score <- c(12, 8,10, 6, 8, 4,10,12, 8, 6,10,14, 9, 7, 9, 5,11,12,
            7,13, 9, 9, 5,11, 8, 7, 3, 8,12,10,13,14,19, 9,16,14)
A <- gl(2, 18, labels = c("a1", "a2"))
B <- rep(gl(3, 6, labels = c("b1", "b2", "b3")), 2)
fit <- aov(score ~ A*B)
cont <- c(1, -1)[A] * c(1, -1, 0)[B]
sum(cont)       # 0
sum(cont*score) # value of the contrast
se.contrast(fit, as.matrix(cont))
(t.stat <- sum(cont*score)/se.contrast(fit, as.matrix(cont)))
summary(fit, split = list(B = 1:2), expand.split = TRUE)
## t.stat^2 is the F value on the A:B: C1 line (with Helmert contrasts)
## Now look at all three interaction contrasts
cont <- c(1, -1)[A] * cbind(c(1, -1, 0), c(1, 0, -1), c(0, 1, -1))[B,]
se.contrast(fit, cont)  # same, due to balance.
rm(A, B, score)


## multi-stratum example where efficiencies play a role
## IGNORE_RDIFF_BEGIN
utils::example(eff.aovlist)
## IGNORE_RDIFF_END
fit <- aov(Yield ~ A + B * C + Error(Block), data = aovdat)
cont1 <- c(-1, 1)[A]/32  # Helmert contrasts
cont2 <- c(-1, 1)[B] * c(-1, 1)[C]/32
cont <- cbind(A = cont1, BC = cont2)
colSums(cont*Yield) # values of the contrasts
se.contrast(fit, as.matrix(cont))
\donttest{# comparison with lme
library(nlme)
fit2 <- lme(Yield ~ A + B*C, random = ~1 | Block, data = aovdat)
summary(fit2)$tTable # same estimates, similar (but smaller) se's.
}}
\keyword{models}
