\name{manova}
\alias{manova}
\title{Multivariate Analysis of Variance}
\description{
  A class for the multivariate analysis of variance.
}
\usage{
manova(\dots)
}
\arguments{
  \item{\dots}{Arguments to be passed to \code{\link{aov}}.}
}
\details{
  Class \code{"manova"} differs from class \code{"aov"} in selecting a
  different \code{summary} method.  Function \code{manova} calls
  \code{\link{aov}} and then add class \code{"manova"} to the result
  object for each stratum.
}
\value{
  See \code{\link{aov}} and the comments in Details here.
}
\note{
  \code{manova} does not support multistratum analysis of variance, so
  the formula should not include an \code{Error} term.
}
\references{
  Krzanowski, W. J. (1988) \emph{Principles of Multivariate Analysis. A
    User's Perspective.} Oxford.

  Hand, D. J. and Taylor, C. C.  (1987)
  \emph{Multivariate Analysis of Variance and Repeated Measures.}
  Chapman and Hall.
}

\seealso{
  \code{\link{aov}}, \code{\link{summary.manova}}, the latter containing
  examples.
}
\keyword{models}
