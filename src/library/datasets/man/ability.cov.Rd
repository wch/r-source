% File src/library/datasets/man/ability.cov.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{ability.cov}
\docType{data}
\alias{ability.cov}
\title{Ability and Intelligence Tests}
\description{
  Six tests were given to 112 individuals. The covariance matrix is
  given in this object.
}
\details{
  The tests are described as
  \describe{
    \item{general:}{a non-verbal measure of general intelligence using
      Cattell's culture-fair test.}
    \item{picture:}{a picture-completion test}
    \item{blocks:}{block design}
    \item{maze:}{mazes}
    \item{reading:}{reading comprehension}
    \item{vocab:}{vocabulary}
  }

  Bartholomew gives both covariance and correlation matrices,
  but these are inconsistent.  Neither are in the original paper.
}
\usage{ability.cov}
\source{
  Bartholomew, D. J. (1987) \emph{Latent Variable Analysis and Factor
    Analysis.} Griffin.

  Bartholomew, D. J.  and Knott, M. (1990) \emph{Latent Variable Analysis
    and Factor Analysis.} Second Edition, Arnold.
}
\references{
  Smith, G. A. and Stanley G. (1983)
  Clocking \eqn{g}: relating intelligence and measures of timed
  performance. \emph{Intelligence}, \bold{7}, 353--368.
}
\examples{\donttest{
require(stats)
(ability.FA <- factanal(factors = 1, covmat = ability.cov))
update(ability.FA, factors = 2)
## The signs of factors and hence the signs of correlations are
## arbitrary with promax rotation.
update(ability.FA, factors = 2, rotation = "promax")
}}
\keyword{datasets}
