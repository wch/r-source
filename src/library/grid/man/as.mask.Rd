% File src/library/grid/man/as.mask.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{as.mask}
\alias{as.mask}
\title{ Define a Soft Mask }
\description{
  Define either an alpha mask or a luminance mask,
  based on a grob.
}
\usage{
as.mask(x, type=c("alpha", "luminance"))
}
\arguments{
  \item{x}{A grob.}
  \item{type}{The type of mask.}
}
\value{
  A \code{"GridMask"} object.
}
\details{
  A mask may be specified for a viewport either directly as
  a grob or using this function.  In the former case, the result
  is an alpha mask.  This function allows the user to define
  a luminance mask instead.
  
  Not all graphics devices support masks and those that do
  may only support one type of mask: for example
  \code{xfig} and \code{pictex} do not support masks and
  Cairo-based devices only support alpha masks.  
}
\author{Paul Murrell}
\seealso{
  \code{\link{viewport}}
}
\examples{
## NOTE: on devices without support for masks normal line segments 
##       will be drawn
grid.newpage()
## Alpha mask
grid.segments(y0=1, y1=0, gp=gpar(col=2, lwd=100))
pushViewport(viewport(mask=circleGrob(gp=gpar(fill=rgb(0,0,0,.5)))))
grid.segments(gp=gpar(col=3, lwd=100))
grid.newpage()
## Luminance mask
grid.segments(y0=1, y1=0, gp=gpar(col=2, lwd=100))
pushViewport(viewport(mask=as.mask(circleGrob(gp=gpar(fill="grey50")),
                                   "luminance")))
grid.segments(gp=gpar(col=3, lwd=100))
}
\keyword{dplot}
