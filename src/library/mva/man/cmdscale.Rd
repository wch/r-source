\name{cmdscale}
\title{Classical (Metric) Multidimensional Scaling}
\usage{
cmdscale(d, k = 2, eig = FALSE)
}
\alias{cmdscale}
\arguments{
  \item{d}{a distance structure such as that returned by \code{dist}
    or a full symmetric matrix containing the dissimilarities.}
  \item{k}{the dimension of the space which the data are to be
    represented in; must be in \eqn{\{1,2, .., n-1\}}.}
  \item{eig}{indicates whether eigenvalues should be returned.}
}
\description{
  Classical multidimensional scaling of a data matrix.
}
\details{
  Multidimensional scaling takes a set of dissimilarities
  and returns a set of points such that the distances between the points
  are approximately equal to the dissimilarities.

  The functions \code{isoMDS} and \code{sammon} in package
  \file{MASS} provide alternative ordination techniques.
}
\value{
  If \code{eig = FALSE}, a matrix with \code{k} columns whose rows give the
  coordinates of the points chosen to represent the dissimilarities.

  Otherwise, a list containing the following components.
  \item{points}{a matrix with \code{k} columns whose rows give the
    coordinates of the points chosen to represent the dissimilarities.}
  \item{eig}{the eigenvalues computed during the scaling process.}
}
\references{
  Cox, F.C. and Cox, M.A.A. (1994)
  \emph{Multidimensional Scaling}.
  Chapman and Hall.

  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).  Chapter 14 of
  \emph{Multivariate Analysis}, London: Academic Press.

  Seber, G. A. F. (1984).
  \emph{Multivariate Observations}.
  New York: Wiley.

  Torgerson, W. S. (1958).
  \emph{Theory and Methods of Scaling}.
  New York: Wiley.
}
\note{
  The S version of this function provides for computing an additional
  ``fiddle'' factor suggested by Torgerson.  \R will provide this
  option from version 1.5.0 on.
}
\seealso{
  \code{\link{dist}}. Also
  \code{\link[MASS]{isoMDS}} and \code{\link[MASS]{sammon}}
  in package \file{MASS}.
}
\examples{
data(eurodist)
loc <- cmdscale(eurodist)
x <- loc[,1]
y <- -loc[,2]
plot(x, y, type="n", xlab="", ylab="", main="cmdscale(eurodist)")
text(x, y, names(eurodist), cex=0.8)

\testonly{
## failed in versions <= 1.4.0 :
cmdscale(eurodist, k = 1)
}
}
\keyword{multivariate}
