\name{grid.frame}
\alias{grid.frame}
\title{ Create a Frame for Packing Objects }
\description{
  This function, together with \code{grid.pack} is part of a
  GUI-builder-like interface to constructing graphical images.
  The idea is that you create a frame with this function then
  use \code{grid.pack} to pack objects into the frame.
}
\usage{
grid.frame(layout=NULL, vp=NULL, gp=gpar(), draw=FALSE)
}
\arguments{
  \item{layout}{ A Grid layout, or NULL. This can be used to initialise
    the frame with a number of rows and columns, with initial widths
    and heights, etc. }
  \item{vp}{ An object of class \code{viewport}, or NULL. }
  \item{gp}{ An object of class \code{gpar};  typically the output from
    a call to the function \code{gpar}.}
\item{draw}{ Should the frame be drawn.  Nothing will actually be
  drawn, but it will put the frame on the display list, which means
  that the output will be dynamically updated as objects are packed
  into the frame.  Possibly useful for debugging.}
}
\value{
  An object of class \code{"grob"}.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.pack}}
}
\examples{
grid.newpage()
gf <- grid.frame(draw=TRUE)
grid.pack(gf, grid.rect(draw=FALSE, gp=gpar(fill="grey")))
grid.pack(gf, grid.text("hi there", draw=FALSE), side="right")
}
\keyword{dplot}
