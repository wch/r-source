\name{screeplot}
\alias{screeplot}
\title{Screeplot of PCA Results}
\usage{
screeplot(x, npcs = min(10, length(x$sdev)),
          type = c("barplot", "lines"), main = deparse(substitute(x)), \dots)
}
\arguments{
  \item{x}{an object of class \code{"princomp"}, as
    from \code{\link{princomp}()}.}
  \item{npcs}{the number of principal components to be plotted.}
  \item{type}{the type of plot.}
  \item{main, \dots}{graphics parameters.}
}
\description{
  \code{screeplot} plots the variances against the number of the
  principal component. This is also the \code{plot} method for class
  \code{"princomp"}.
}
\references{
  Mardia, K. V., J. T. Kent and J. M. Bibby (1979).
  \emph{Multivariate Analysis}, London: Academic Press.

  Venables, W. N. and B. D. Ripley (1997, 9).
  \emph{Modern Applied Statistics with S-PLUS}, Springer-Verlag.
}
\seealso{
  \code{\link{princomp}}.
}
\examples{
## The variances of the variables in the
## USArrests data vary by orders of magnitude, so scaling is appropriate
data(USArrests)
(pc.cr <- princomp(USArrests, cor = TRUE))  # inappropriate
screeplot(pc.cr)

data(Harman74.cor)
fit <- princomp(covmat=Harman74.cor)
screeplot(fit)
screeplot(fit, npcs=24, type="lines")
}
\keyword{multivariate}
