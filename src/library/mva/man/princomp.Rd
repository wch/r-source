\name{princomp}
\alias{princomp}
\alias{plot.princomp}
\alias{print.princomp}
\alias{predict.princomp}
\alias{summary.princomp}
\alias{loadings}
\alias{screeplot}
\title{Principal Components Analysis}
\usage{
princomp(x, cor = FALSE, scores = TRUE, covmat = NULL,
         subset = rep(TRUE, nrow(as.matrix(x))))
loadings(x)
plot(x, npcs = min(10, length(x$sdev)),
          type = c("barplot", "lines"), \dots)
screeplot(x, npcs = min(10, length(x$sdev)),
          type = c("barplot", "lines"), \dots)

print(x, \dots)  summary(object)  predict(object, \dots)
}
\arguments{
  \item{x}{a matrix (or data frame) which provides the data for the
    principal components analysis.}
  \item{cor}{a logical value indicating whether the calculation should
    use the correlation matrix or the covariance matrix.}
  \item{scores}{a logical value indicating whether the score on each
    principal component should be calculated.}
  \item{covmat}{a covariance matrix, or a covariance list as returned by
    \code{\link{cov.wt}}, \code{\link{cov.mve}} or \code{\link{cov.mcd}}.
    If supplied, this is used rather than the covariance matrix of
    \code{x}.}
  \item{subset}{a vector used to select rows (observations) of the
    data matrix \code{x}.}

  \item{x, object}{an object of class \code{"princomp"}, as
    from \code{princomp()}.}
  \item{npcs}{the number of principal components to be plotted.}
  \item{type}{the type of plot.}
  \item{...}{graphics parameters.}
}
\description{
  \code{princomp} performs a principal components analysis on the given
  data matrix and returns the results as an object of class
  \code{princomp}.

  \code{loadings} extracts the loadings.

  \code{screeplot} plots the variances against the number of the
  principal component. This is also the \code{plot} method.
}
\value{
  \code{princomp} returns a list with class \code{"princomp"}
  containing the following components:
  \item{sdev}{the standard deviations of the principal components.}
  \item{loadings}{the matrix of variable loadings (i.e., a matrix
    whose columns contain the eigenvectors).}
  \item{center}{the means that were subtracted.}
  \item{scale}{the scalings applied to each variable.}
  \item{n.obs}{the number of observations.}
  \item{scores}{if \code{scores = TRUE}, the scores of the supplied
    data on the principal components.}
  \item{call}{the matched call.}
}
\details{
  The calculation is done using \code{\link{eigen}} on the correlation or
  covariance matrix, as determined by \code{\link{cor}}.  This is done for
  compatibility with the S-PLUS result.  A preferred method of
  calculation is to use svd on \code{x}, as is done in \code{prcomp}.

  Note that the default calculation uses divisor \code{N} for the
  covariance matrix.

  The \code{\link{print}} method for the these objects prints the
  results in a nice format and the \code{\link{plot}} method produces
  a scree plot.
}
\references{
  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
  \emph{Multivariate Analysis}, London: Academic Press.

  Venables, W. N. and B. D. Ripley (1997, 9).
  \emph{Modern Applied Statistics with S-PLUS}, Springer-Verlag.
}
\seealso{
  \code{\link{prcomp}}, \code{\link{cor}}, \code{\link{cov}},
  \code{\link{eigen}}.
}
\examples{
## The variances of the variables in the
## USArrests data vary by orders of magnitude
data(USArrests)
(pc.cr <- princomp(USArrests))
princomp(USArrests, cor = TRUE) # =^= prcomp(USArrests, scale=TRUE)
## Similar, but different:
princomp(scale(USArrests, scale = TRUE, center = TRUE), cor = FALSE)

summary(pc.cr <- princomp(USArrests))
loadings(pc.cr)
plot(pc.cr) # does a screeplot.
biplot(pc.cr)
}
\keyword{multivariate}
