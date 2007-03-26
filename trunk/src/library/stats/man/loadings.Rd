\name{loadings}
\alias{loadings}
\alias{print.loadings}
\alias{print.factanal}
\title{Print Loadings in Factor Analysis}
\description{
  Extract or print loadings in factor analysis (or principal
  components analysis).
}
\usage{
loadings(x)

\method{print}{loadings}(x, digits = 3, cutoff = 0.1, sort = FALSE, \dots)

\method{print}{factanal}(x, digits = 3, \dots)
}
\arguments{
  \item{x}{an object of class \code{"factanal"} or \code{"princomp"}
    or the \code{loadings} component of such an object.}
  \item{digits}{number of decimal places to use in printing uniquenesses
    and loadings.}
  \item{cutoff}{loadings smaller than this (in absolute value) are suppressed.}
  \item{sort}{logical. If true, the variables are sorted by their
    importance on each factor.  Each variable with any loading larger
    than 0.5 (in modulus) is assigned to the factor with the largest
    loading, and the variables are printed in the order of the factor
    they are assigned to, then those unassigned.}
  \item{\dots}{further arguments for other methods, such as
    \code{cutoff} and \code{sort} for \code{print.factanal}.}
}

\seealso{
  \code{\link{factanal}}, \code{\link{princomp}}
}

\keyword{multivariate}
\keyword{print}
