% file dummy.coef.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{dummy.coef}
\title{Extract Coefficients in Original Coding}
\usage{
dummy.coef(object, ...)
dummy.coef.lm(object, use.na = FALSE)
dummy.coef.aovlist(object, use.na = FALSE)
}
\alias{dummy.coef}
\alias{dummy.coef.lm}
\alias{dummy.coef.aovlist}
\alias{print.dummy.coef}
\alias{print.dummy.coef.list}
\arguments{
 \item{object}{a linear model fit}
 \item{use.na}{logical flag for coefficients in a singular model. If
   \code{use.na} is true, undetermined coefficients will be missing; if
   false they will get one possible value.}
}
\description{
 This extracts coefficients in terms of the original levels of the
 coefficients rather than the coded variables.
}
\details{
 A fitted linear model has coefficients for the contrasts of the factor
 terms, usually one less in number than the number of levels. This
 function re-expresses the coefficients in the original coding; as the
 coefficients will have been fitted in the reduced basis, any implied
 constraints (e.g. zero sum for \code{contr.helmert} or \code{contr.sum}
 will be respected. There will be little point in using
 \code{dummy.coef} for \code{contr.treatment} contrasts, as the missing
 coefficients are by definition zero.
}
\value{
  A list giving for each term the values of the coefficients. For a
  multistratum \code{aov} model, such a list for each stratum.
}
\author{B.D. Ripley}

\section{WARNING}{This function is intended for human inspection of the
  output: it should not be used for calculations. Use coded variables
  for all calculations.

  The results differ from S for singular values, where S can be incorrect.
  }

\seealso{\code{\link{aov}},  \code{\link{model.tables}}}

\examples{
options(contrasts=c("contr.helmert", "contr.poly"))
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)
npk.aov <- aov(yield ~ block + N*P*K, npk)
dummy.coef(npk.aov)

npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
dummy.coef(npk.aovE)
}
\keyword{models}
