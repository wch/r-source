\name{dist}
\title{Distance Matrix Computation}
\usage{
dist(x, method = "euclidean", diag = FALSE, upper = FALSE)

print.dist(dist.obj, diag = NULL, upper = NULL)
as.matrix.dist(dist.obj)
as.dist(m, diag = NULL, upper = NULL)
}
\alias{dist}
\alias{print.dist}
\alias{as.matrix.dist}
\alias{names.dist}
\alias{names<-.dist}
\alias{as.dist}
\arguments{
  \item{x}{A matrix or (data frame).  Distances between the rows of
    \code{x} will be computed.}
  \item{method}{The distance measure to be used. This must be one of
    \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, 
    \code{"canberra"} or \code{"binary"}.
    Any unambiguous substring can be given.}
  \item{diag}{A logical value indicating whether the diagonal of the
    distance matrix should be printed by \code{print.dist}.}
  \item{upper}{A logical value indicating whether the upper triangle of the
      distance matrix should be printed by \code{print.dist}.}
  \item{m}{A distance matrix to be converted to a dist object (only
      lower triangle is used, the rest is ignored).}
}
\description{
  This function computes and returns the distance matrix computed by
  using the specified distance measure to compute the distances between
  the rows of a data matrix.
}
\details{
  Available distance measures are (written for two vectors \eqn{x} and
  \eqn{y}): 
  \itemize{
    \item Euclidean: Usual square distance between the two vectors (2 norm).
    \item Maximum: Maximum distance between two components of \eqn{x}
    and \eqn{y} (supremum norm)
    \item Manhattan: Absolute distance between the two vectors (1
    norm).
    \item Canberra: \eqn{\sum_i |x_i - y_i| / |x_i + y_i|}{sum(|x_i - y_i| / |x_i + y_i|)}
    \item Count the number of different bits in \eqn{x} and \eqn{y}
    where at least one of the two bits is 1, i.e., components where both
    bits are zero are ignored.
  }
  
  The functions \code{as.matrix.dist()} and \code{as.dist()} can be used
  for conversion between objects of class \code{"dist"} and conventional
  distance matrices and vice versa.
}
\value{
  The lower triangle of the distance matrix stored by columns in a
  single vector.  The vector has the attributes \code{"Size"},
  \code{"Diag"}, \code{"Upper"}, \code{"Labels"} and \code{"class"} equal
  to \code{"dist"}.
}
\references{
  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
  \emph{Multivariate Analysis}, London: Academic Press.
}
\seealso{
  \code{\link{hclust}}.
}
\examples{
x <- matrix(rnorm(100),nrow=5)
dist(x)
dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
as.dist(m)
}
\keyword{multivariate}
\keyword{cluster}
