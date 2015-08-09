% File src/library/stats/man/eff.aovlist.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{eff.aovlist}
\title{Compute Efficiencies of Multistratum Analysis of Variance}
\usage{
eff.aovlist(aovlist)
}
\alias{eff.aovlist}
\arguments{
  \item{aovlist}{
    The result of a call to \code{aov} with an \code{Error} term.
  }
}
\description{
  Computes the efficiencies of fixed-effect terms in an analysis of
  variance model with multiple strata.
}
\details{
  Fixed-effect terms in an analysis of variance model with multiple strata
  may be estimable in more than one stratum, in which case there is less
  than complete information in each.  The efficiency for a term
  is the fraction of the maximum possible precision (inverse variance)
  obtainable by estimating in just that stratum.  Under the assumption
  of balance, this is the same for all contrasts involving that term.

  This function is used to pick strata in which to estimate terms in
  \code{\link{model.tables.aovlist}} and
  \code{\link{se.contrast.aovlist}}.

  In many cases terms will only occur in one stratum, when all the
  efficiencies will be one: this is detected and no further calculations
  are done.

  The calculation used requires orthogonal contrasts for each term, and
  will throw an error if non-orthogonal contrasts (e.g., treatment
  contrasts or an unbalanced design) are detected.
}
\value{
  A matrix giving for each non-pure-error stratum (row) the efficiencies
  for each fixed-effect term in the model.
}
\references{
  Heiberger, R. M. (1989)
  \emph{Computation for the Analysis of Designed Experiments}.  Wiley.
}

\seealso{
  \code{\link{aov}}, \code{\link{model.tables.aovlist}},
  \code{\link{se.contrast.aovlist}}
}

\examples{
## An example from Yates (1932),
## a 2^3 design in 2 blocks replicated 4 times

Block <- gl(8, 4)
A <- factor(c(0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,
              0,1,0,1,0,1,0,1,0,1,0,1))
B <- factor(c(0,0,1,1,0,0,1,1,0,1,0,1,1,0,1,0,0,0,1,1,
              0,0,1,1,0,0,1,1,0,0,1,1))
C <- factor(c(0,1,1,0,1,0,0,1,0,0,1,1,0,0,1,1,0,1,0,1,
              1,0,1,0,0,0,1,1,1,1,0,0))
Yield <- c(101, 373, 398, 291, 312, 106, 265, 450, 106, 306, 324, 449,
           272, 89, 407, 338, 87, 324, 279, 471, 323, 128, 423, 334,
           131, 103, 445, 437, 324, 361, 302, 272)
aovdat <- data.frame(Block, A, B, C, Yield)

old <- getOption("contrasts")
options(contrasts = c("contr.helmert", "contr.poly"))
(fit <- aov(Yield ~ A*B*C + Error(Block), data = aovdat))
eff.aovlist(fit)
options(contrasts = old)
}
\keyword{models}
