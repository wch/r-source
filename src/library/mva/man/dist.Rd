\name{dist}
\title{Distance Matrix Computation}
\usage{
dist(x, method = "euclidian", diag = FALSE)

print.dist(dist.obj)
}
\arguments{
  \item{x}{a matrix or (data frame).  Distances between the rows of
    \code{x} will be computed.}
  \item{method}{the distance measure to be used. This must be one of
    \code{"euclidian"}, \code{"maximum"}, \code{"manhattan"}, 
    \code{"canberra"} or \code{"binary"}.
    Any unambiguous substring can be given.}
  \item{diag}{a logical value indicating whether the diagonal of the
    distance matrix should be included in the result.}
}
\description{
  This function computes and returns the distance matrix computed by
  using the specified distance measure to compute the distances between
  the rows of \code{x}.
}
\value{
  The lower triangle of the distance matrix (with or without its
  diagonal as specified by \code{diag}) and stored by columns in a
  single vector.  The vector has the attributes \code{"size"},
  \code{"diag"}, \code{"labels"} and \code{class} equal to "dist".
}
\references{
  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
  \emph{Multivariate Analysis}, London: Academic Press.
}
\seealso{
  \code{\link{hclust}}.
}
\examples{
# There are no examples yet.
}
