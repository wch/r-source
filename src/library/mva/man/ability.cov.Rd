\name{ability.cov}
\alias{ability.cov}
\non_function{}
\title{Ability and Intelligence Tests}
\description{
  Six tests were given to 112 individuals. The covariance matrix is
  given in this object.
}
\details{
  Bartholomew gives both covariance and correlation matrices,
  but these are inconsistent.  Neither are in the original paper.
}
\usage{data(ability.cov)}
\source{
  Barthlomew, D. J. (1987) \emph{Latent Variable Analysis and Factor
    Analysis.} Griffin.  
}
\references{
  Smith, G. A. and Stanley G. (1983)
  Clocking \eqn{g}: relating intelligence and measures of timed
  performance. \emph{Intelligence}, \bold{7}, 353--368.
}
\examples{
data(ability.cov)
(ability.FA <- factanal(factors = 1, covmat=ability.cov))
update(ability.FA, factors=2)
update(ability.FA, factors=2, rotation="promax")
}
\keyword{datasets}
