\name{dist}
\title{Distance Matrix Computation}
\usage{
dist(x, method = "euclidean", diag = FALSE, upper = FALSE)

print.dist(x, diag = NULL, upper = NULL, \dots)
as.matrix.dist(x)
as.dist(m, diag = FALSE, upper = FALSE)
}
\alias{dist}
\alias{print.dist}
\alias{format.dist}
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
  \item{m}{A matrix of distances to be converted to a \code{"dist"}
    object (only the lower triangle is used, the rest is ignored).}
  \item{\dots}{further arguments, passed to the (next) \code{print} method.}
}
\description{
  This function computes and returns the distance matrix computed by
  using the specified distance measure to compute the distances between
  the rows of a data matrix.
}
\details{
  Available distance measures are (written for two vectors \eqn{x} and
  \eqn{y}):
  \describe{
    \item{\code{euclidean}:}{Usual square distance between the two
      vectors (2 norm).}

    \item{\code{maximum}:}{Maximum distance between two components of \eqn{x}
      and \eqn{y} (supremum norm)}

    \item{\code{manhattan}:}{Absolute distance between the two vectors
      (1 norm).}

    \item{\code{canberra}:}{\eqn{\sum_i |x_i - y_i| / |x_i + y_i|}{%
	sum(|x_i - y_i| / |x_i + y_i|)}.  Terms with zero numerator and
      denominator are omitted from the sum and treated as if the values
      were missing.
    }

    \item{\code{binary}:}{(aka \emph{asymmetric binary}): The vectors
      are regarded as binary bits, so non-zero elements are `on' and zero
      elements are `off'.  The distance is the \emph{proportion} of
      bits in which only one is on amongst those in which at least one is on.}
  }

  Missing values are allowed, and are excluded from all computations
  involving the rows within which they occur.  If some columns are
  excluded in calculating a Euclidean, Manhattan or Canberra distance,
  the sum is scaled up proportionally to the number of columns used.
  If all pairs are excluded when calculating a particular distance,
  the value is \code{NA}.

  The functions \code{as.matrix.dist()} and \code{as.dist()} can be used
  for conversion between objects of class \code{"dist"} and conventional
  distance matrices and vice versa.
}
\value{
  An object of class \code{"dist"}.

  The lower triangle of the distance matrix stored by columns in a
  single vector.  The vector has the attributes \code{"Size"},
  \code{"Diag"}, \code{"Upper"}, \code{"Labels"} and \code{"class"} equal
  to \code{"dist"}.
}
\references{
  Mardia, K. V., Kent, J. T. and Bibby, J. M. (1979)
  \emph{Multivariate Analysis.} London: Academic Press.
}
\seealso{
  \code{\link{hclust}}.
}
\examples{
x <- matrix(rnorm(100), nrow=5)
dist(x)
dist(x, diag = TRUE)
dist(x, upper = TRUE)
m <- as.matrix(dist(x))
d <- as.dist(m)
stopifnot(d == dist(x))
names(d) <- LETTERS[1:5]
print(d, digits = 3)

## example of binary and canberra distances.
x <- c(0, 0, 1, 1, 1, 1)
y <- c(1, 0, 1, 1, 0, 1)
dist(rbind(x,y), method="binary")
## answer 0.4 = 2/5
dist(rbind(x,y), method="canberra")
## answer 2 * (6/5)
}
\keyword{multivariate}
\keyword{cluster}
