\name{summary.princomp}
\alias{summary.princomp}
\title{Summary method for Principal Components Analysis}
\usage{
\method{summary}{princomp}(object, loadings = FALSE, cutoff = 0.1,
        digits = 3, \dots) 

}
\arguments{
  \item{object}{an object of class \code{"princomp"}, as
    from \code{princomp()}.}
  \item{loadings}{logical. Should loadings be included?}
  \item{cutoff}{numeric. Loadings below this cutoff in absolute value
    are shown as blank in the output.}
  \item{digits}{the number of significant digits to be used in listing
    loadings.}
  \item{\dots}{arguments to be passed to or from other methods.}
}
\description{
  The \code{\link{summary}} method for class \code{"princomp"}.
}
\value{
  \code{object}, invisibly.
}
\seealso{
  \code{\link{princomp}}
}
\examples{
data(USArrests)
summary(pc.cr <- princomp(USArrests, cor=TRUE))
summary(princomp(USArrests, cor=TRUE),
        loadings = TRUE, cutoff = 0.2, digits = 2)
}
\keyword{multivariate}
