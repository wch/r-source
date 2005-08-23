\name{unit}
\alias{unit}
\title{Function to Create a Unit Object}
\description{
  This function creates a unit object --- a vector of unit values.
  A unit value is typically just a single numeric value with an
  associated unit.
}
\usage{
unit(x, units, data=NULL)
}
\arguments{
  \item{x}{A numeric vector.}
  \item{units}{A character vector specifying the units for the
    corresponding numeric values.}
  \item{data}{This argument is used to supply extra information
    for special \code{unit} types.}
}
\details{
  Unit objects allow the user to specify locations and dimensions
  in a large number of different coordinate systems.  All drawing
  occurs relative to a viewport and the \code{units} specifies
  what coordinate system to use within that viewport.
  
  Possible \code{units} (coordinate systems) are:
  \describe{
    \item{\code{"npc"}}{Normalised Parent Coordinates (the default).
      The origin of the viewport is (0, 0) and the viewport has a width
      and height of 1 unit.  For example, (0.5, 0.5) is the centre of
      the viewport.}
    \item{\code{"cm"}}{Centimetres.}
    \item{\code{"inches"}}{Inches.  1 in = 2.54 cm.}
    \item{\code{"mm"}}{Millimetres.  10 mm = 1 cm.}
    \item{\code{"points"}}{Points.  72.27 pt = 1 in.}
    \item{\code{"picas"}}{Picas.  1 pc = 12 pt.}
    \item{\code{"bigpts"}}{Big Points.  72 bp = 1 in.}
    \item{\code{"dida"}}{Dida. 1157 dd = 1238 pt.}
    \item{\code{"cicero"}}{Cicero.  1 cc = 12 dd.}
    \item{\code{"scaledpts"}}{Scaled Points.  65536 sp = 1 pt.}
    \item{\code{"lines"}}{Lines of text.  Locations and dimensions
      are in terms of multiples of the default text size of the viewport
      (as specified by the viewport's \code{fontsize} and \code{lineheight}).}
    \item{\code{"char"}}{Multiples of nominal font height of the
      viewport (as specified by the viewport's \code{fontsize}).}
    \item{\code{"native"}}{Locations and dimensions are relative to
      the viewport's \code{xscale} and \code{yscale}.}
    \item{\code{"snpc"}}{Square Normalised Parent Coordinates.
      Same as Normalised Parent Coordinates, except gives the same answer for
      horizontal and vertical locations/dimensions.  It uses the
      \emph{lesser} of npc-width and npc-height.  This is useful for making
      things which are a proportion of the viewport, but have to be square 
      (or have a fixed aspect ratio).}
    \item{\code{"strwidth"}}{Multiples of the width of the string
      specified in the \code{data} argument.  The font size is
      determined by the pointsize of the viewport.}
    \item{\code{"strheight"}}{Multiples of the height of the string
      specified in the \code{data} argument.  The font size is
      determined by the pointsize of the viewport.}
    \item{\code{"grobwidth"}}{Multiples of the width of the grob
      specified in the \code{data} argument.}
    \item{\code{"grobheight"}}{Multiples of the height of the grob
      specified in the \code{data} argument.}
  }

  A special \code{units} value of \code{"null"} is also allowed,
  but only makes sense when used in specifying widths of columns
  or heights of rows in grid layouts (see \code{\link{grid.layout}}).
  
  The \code{data} argument must be a list when the \code{unit.length()}
  is greater than 1.  For example,
  \code{unit(rep(1, 3), c("npc", "strwidth", "inches"),
        data=list(NULL, "my string", NULL))}.
      
  It is possible to subset unit objects in the normal way
  (e.g., \code{unit(1:5, "npc")[2:4]}), but a special function
  \code{unit.c} is provided for combining unit objects.

  Certain arithmetic and summary operations are defined for
  unit objects.  In particular, it is possible to add and subtract
  unit objects (e.g., \code{unit(1, "npc") - unit(1, "inches")}),
  and to specify the minimum or maximum of a list
  of unit objects (e.g., \code{min(unit(0.5, "npc"), unit(1,
    "inches"))}).
}
\section{WARNING}{
  A special function \code{unit.length} is provided for
  determining the number of unit values in a unit object.

  The \code{length} function will work in some cases, but in general
  will not give the right answer.

  There is also a special function \code{unit.c} for concatenating
  several unit objects.

  The \code{c} function will not give the right answer.

  There used to be \code{"mylines"}, \code{"mychar"}, \code{"mystrwidth"},
  \code{"mystrheight"} units.  These will still be accepted, but
  work exactly the same as
  \code{"lines"}, \code{"char"}, \code{"strwidth"},
  \code{"strheight"}.
}
\value{
  An object of class \code{"unit"}. 
}
\author{Paul Murrell}
\seealso{
  \code{\link{unit.c}} and
  \code{\link{unit.length}}
}

\examples{
unit(1, "npc")
unit(1:3/4, "npc")
unit(1:3/4, "npc") + unit(1, "inches")
min(unit(0.5, "npc"), unit(1, "inches"))
unit.c(unit(0.5, "npc"), unit(2, "inches") + unit(1:3/4, "npc"),
       unit(1, "strwidth", "hi there"))
}
\keyword{dplot}
