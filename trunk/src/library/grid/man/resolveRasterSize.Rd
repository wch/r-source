\name{resolveRasterSize}
\alias{resolveRasterSize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Utility function to resolve the size of a raster grob
}
\description{
  Determine the width and height of a raster grob when one or both
  are not given explicitly.

  The result depends on both the aspect ratio of the raster image
  and the aspect ratio of the physical drawing context, so the
  result is only valid for the drawing context in which this
  function is called.
}
\usage{
resolveRasterSize(x)
}
\arguments{
  \item{x}{ A raster grob }
}
\details{
  A raster grob can be specified with width and/or height of
  \code{NULL}, which means that the size at which the raster is
  drawn will be decided at drawing time.
  }
\value{
A raster grob, with explicit width and height.
}
\seealso{
\code{\link{grid.raster}}
}
\examples{
# Square raster
rg <- rasterGrob(matrix(0))
# Fill the complete page (if page is square)
grid.newpage()
resolveRasterSize(rg)$height
grid.draw(rg)
# Forced to fit tall thin region
grid.newpage()
pushViewport(viewport(width=.1))
resolveRasterSize(rg)$height
grid.draw(rg)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dplot }
