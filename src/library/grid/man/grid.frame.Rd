\name{grid.frame}
\alias{grid.frame}
\alias{frameGrob}
\title{ Create a Frame for Packing Objects }
\description{
  These functions, together with \code{grid.pack},
  \code{grid.place}, \code{packGrob}, and \code{placeGrob} are part of a
  GUI-builder-like interface to constructing graphical images.
  The idea is that you create a frame with this function then
  use \code{grid.pack} or whatever to pack/place objects into the frame.
}
\usage{
grid.frame(layout=NULL, name=NULL, gp=gpar(), vp=NULL, draw=TRUE)
frameGrob(layout=NULL, name=NULL, gp=gpar(), vp=NULL)
}
\arguments{
  \item{layout}{ A Grid layout, or NULL. This can be used to initialise
    the frame with a number of rows and columns, with initial widths
    and heights, etc. }
  \item{name}{ A character identifier. }
  \item{vp}{ An object of class \code{viewport}, or NULL. }
  \item{gp}{ An object of class \code{gpar};  typically the output from
    a call to the function \code{gpar}.}
\item{draw}{ Should the frame be drawn.  }
}
\details{
  Both functions create a frame grob (a graphical object describing a
  frame), but only \code{grid.frame()}
  draws the frame (and then only if \code{draw} is \code{TRUE}).
  Nothing will actually be
  drawn, but it will put the frame on the display list, which means
  that the output will be dynamically updated as objects are packed
  into the frame.  Possibly useful for debugging.
}
\value{
  A frame grob.  \code{grid.frame()} returns the value invisibly.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.pack}}
}
\examples{
grid.newpage()
grid.frame(name="gf", draw=TRUE)
grid.pack("gf", rectGrob(gp=gpar(fill="grey")), width=unit(1, "null"))
grid.pack("gf", textGrob("hi there"), side="right")
}
\keyword{dplot}
