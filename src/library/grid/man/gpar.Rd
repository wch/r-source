\name{gpar}
\alias{gpar}
\title{Function to produce a Graphical Parameter Object}
\description{
  This function returns an object of class \code{"gpar"}.  This is
  basically a list of name-value pairs.
}
\usage{
gpar(...)
}
\arguments{
  \item{\dots}{ Any number of named arguments. }
}
\details{
  All grid viewports and (predefined) graphical objects have a slot
  called \code{gp}, which contains a \code{"gpar"} object.  When
  a viewport is pushed onto the viewport stack and when a graphical object
  is drawn, the settings in the \code{"gpar"} object are enforced.
  In this way, the graphical output is modified by the \code{gp}
  settings until the graphical object has finished drawing, or until the
  viewport is popped off the viewport stack, or until some other
  viewport or graphical object is pushed or begins drawing.

  Valid parameter names are:
  \tabular{ll}{
    col \tab Colour for lines and borders. \cr
    fill \tab Colour for filling rectangles, polygons, ... \cr
    lty \tab Line type \cr
    lwd \tab Line width \cr
    fontsize \tab The size of text (in points) \cr
    fontfamily \tab The font family \cr
    fontface \tab The font face (bold, italic, ...) \cr
    lineheight \tab The height of a line as a multiple of fontsize \cr
    font \tab Font face (alias for fontface;  for
    backward compatibility) \cr
  }

  For most devices, the \code{fontfamily} is specified when the device
  is first opened and may not be changed thereafter -- i.e., specifying
  a different font family via \code{fontfamily} will be ignored.  This
  will hopefully change in future versions of R.  Also, there is an
  important exception:  \code{fontfamily} may be used to specify one
  of the Hershey Font families (e.g., \code{HersheySerif})
  and this specification will be honoured
  on all devices.

  The specification of \code{fontface} follows the R base graphics
  standard:  1 = plain, 2 = bold, 3 = italic, 4 = bold italic.
  
  Specifying the value \code{NULL} for a parameter is the same as not
  specifying any value for that parameter, except for \code{col} and
  \code{fill}, where \code{NULL} indicates not to draw a border or
  not to fill an area (respectively).

  All parameter values can be vectors of multiple values.  (This will
  not always make sense -- for example, viewports will only take
  notice of the first parameter value.)
}
\value{
  An object of class \code{"gpar"}.
}
\seealso{
  \code{\link{Hershey}}.
}
\author{Paul Murrell}
\examples{
gpar(col = "red")
gpar(col = "blue", lty = "solid", lwd = 3, fontsize = 16)
grid.newpage()
vp <- viewport(w = .8, h = .8, gp = gpar(col="blue"))
grid.collection(grid.rect(gp = gpar(col="red"), draw = FALSE),
                grid.text(paste("The rect is its own colour (red)",
                                "but this text is the colour",
                                "set by the collection (green)", sep = "\n"),
                          draw = FALSE),
                gp = gpar(col="green"), vp = vp)
grid.text("This text is the colour set by the viewport (blue)",
          y = 1, just = c("center", "bottom"),
          gp = gpar(fontsize=20), vp = vp)
grid.newpage()
## example with multiple values for a parameter
push.viewport(viewport())
grid.points(1:10/11, 1:10/11, gp = gpar(col=1:10))
pop.viewport()
}
\keyword{ dplot }
