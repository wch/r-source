\name{as.hclust}
\alias{as.hclust}
\alias{as.hclust.twins}
\title{Convert Objects to Class hclust}
\usage{
as.hclust(x, ...)
as.hclust.twins(x)
}
\arguments{
 \item{x}{Hierarchical clustering object}
}
\description{
  Converts objects from other hierarchical clustering functions to
  class \code{"hclust"}.
}
\details{
  Currently there is only support for converting objects of
  class \code{"twins"} as produced by the functions \code{diana} and
  \code{agnes} from the package \file{cluster}.
}
\value{
  An object of class \code{"hclust"}.
}
\seealso{
  \code{\link{hclust}},\code{\link{diana}}, \code{\link{agnes}}
}
\examples{
\dontrun{
x <- matrix(rnorm(30), ncol=3)
hc <- hclust(dist(x), method="complete")

library(cluster)
ag <- agnes(x, method="complete")

x11()
par(mfrow=c(1,2))
plot(hc)
mtext("hclust", side=1)
plot(as.hclust(ag))
mtext("agnes", side=1)
}
}
\keyword{multivariate}
\keyword{cluster}

