\name{prcomp}
\title{Principal Components Analysis}
\usage{
prcomp(x=, scale=FALSE, use="all.obs")

print.prcomp(prcomp.obj)
plot.prcomp(prcomp.obj)
}
\alias{print.prcomp}
\alias{plot.prcomp}
\alias{prcomp}
\arguments{
\item{x}{a matrix (or data frame) which provides the data
for the principal components analysis.}
\item{scale}{a logical value indicating whether the variables should
be scaled to have unit variance before the analysis takes place.}
\item{use}{the strategy to use for dealing with missing observations.
The possible values are \code{"all.obs"}, \code{"complete.obs"},
or \code{"pairwise.complete.obs"}.
An unambiguous substring can be used.}
}
\description{
This function performs a principal components analysis on
the given data matrix and returns the results as a
\code{prcomp} object.  The print method for the these
objects prints the results in a nice format and the
plot method produces a scree plot.
}
\value{
\code{prcomp} returns an list with class \code{"prcomp"}
containing the following components:
\item{var}{the variances of the principal components
(i.e. the eigenvalues)}
\item{load}{the matrix of variable loadings (i.e. a matrix
whose columns contain the eigenvectors).}
\item{scale}{the value of the \code{scale} argument.}
}
\references{
Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
\emph{Multivariate Analysis}, London: Academic Press.
}
\seealso{
\code{\link{cor}}, \code{\link{cov}}, \code{\link{eigen}}.
}
\examples{
# the variances of the variables in the
# crimes data vary by orders of magnitude
data(crimes)
prcomp(crimes)
prcomp(crimes,scale=TRUE)
}
\keyword{multivariate}
