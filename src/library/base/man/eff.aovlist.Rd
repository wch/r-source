% file eff.aovlist.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{eff.aovlist}
\title{Compute Efficiencies of Multistratum Analysis of Variance}
\usage{
eff.aovlist(aovlist)
}
\alias{eff.aovlist}
\arguments{
 \item{aovlist}{The result of a call to \code{aov} with a \code{Error} term.}
}
\description{
 ~~ A concise (1-5 lines) description of what the function does. ~~
}
\details{
 Fixed-effect terms in an analysis of variance model with multiple strata
 may be estimable in more than one stratum, in which case there is less
 than complete information in each. The efficiency is the fraction of
 the maximum possible precision (inverse variance) obtainable by
 estimating in just that stratum.

 This is used to pick strata in which to estimate terms in
 \code{model.tables.aovlist} and elsewhere.
}
\value{
  A matrix giving for each non-pure-error stratum (row) the efficiencies
  for each fixed-effect term in the model.
}
\author{B.D. Ripley}
\seealso{\code{\link{aov}}, \code{\link{model.tables.aovlist}},
  \code{\link{se.contrast.aovlist}} }

\examples{
## for balanced designs all efficiencies are zero or one.
## so as a statistically meaningless test:
options(contrasts=c("contr.helmert", "contr.poly"))
## From Venables and Ripley (1997) p.210.
N <- c(0,1,0,1,1,1,0,0,0,1,1,0,1,1,0,0,1,0,1,0,1,1,0,0)
P <- c(1,1,0,0,0,1,0,1,1,1,0,0,0,1,0,1,1,0,0,1,0,1,1,0)
K <- c(1,0,0,1,0,1,1,0,0,1,0,1,0,1,1,0,0,0,1,1,1,0,1,0)
yield <- c(49.5,62.8,46.8,57.0,59.8,58.5,55.5,56.0,62.8,55.8,69.5,
55.0, 62.0,48.8,45.5,44.2,52.0,51.5,49.8,48.8,57.2,59.0,53.2,56.0)

npk <- data.frame(block=gl(6,4), N=factor(N), P=factor(P),
                  K=factor(K), yield=yield)
npk.aovE <- aov(yield ~  N*P*K + Error(block), npk)
eff.aovlist(npk.aovE)
}
\keyword{models}
