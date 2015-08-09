% File src/library/stats/man/as.hclust.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{as.hclust}
\alias{as.hclust}
\alias{as.hclust.default}
\alias{as.hclust.twins}
\title{Convert Objects to Class hclust}
\usage{
as.hclust(x, \dots)
}
\arguments{
  \item{x}{Hierarchical clustering object}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Converts objects from other hierarchical clustering functions to
  class \code{"hclust"}.
}
\details{
  Currently there is only support for converting objects of
  class \code{"twins"} as produced by the functions \code{diana} and
  \code{agnes} from the package \CRANpkg{cluster}.  The default method
  throws an error unless passed an \code{"hclust"} object.
}
\value{
  An object of class \code{"hclust"}.
}
\seealso{
  \code{\link{hclust}}, and from package \CRANpkg{cluster},
  \code{\link[cluster]{diana}} and \code{\link[cluster]{agnes}}
}
\examples{
x <- matrix(rnorm(30), ncol = 3)
hc <- hclust(dist(x), method = "complete")

\donttest{if(require("cluster", quietly = TRUE)) {# is a recommended package
  ag <- agnes(x, method = "complete")
  hcag <- as.hclust(ag)
  ## The dendrograms order slightly differently:
  op <- par(mfrow = c(1,2))
  plot(hc) ;  mtext("hclust", side = 1)
  plot(hcag); mtext("agnes",  side = 1)
  detach("package:cluster")
}}
}
\keyword{multivariate}
\keyword{cluster}
