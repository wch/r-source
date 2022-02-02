% File src/library/grid/man/grid.pack.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid.pack}
\alias{grid.pack}
\alias{packGrob}
\title{ Pack an Object within a Frame }
\description{
  these functions, together with \code{grid.frame} and \code{frameGrob}
  are part of a
  GUI-builder-like interface to constructing graphical images.
  The idea is that you create a frame with \code{grid.frame} or
  \code{frameGrob} then
  use these functions to pack objects into the frame.
}
\usage{
grid.pack(gPath, grob, redraw = TRUE, side = NULL,
          row = NULL, row.before = NULL, row.after = NULL,
          col = NULL, col.before = NULL, col.after = NULL,
          width = NULL, height = NULL,
          force.width = FALSE, force.height = FALSE, border = NULL,
          dynamic = FALSE)

packGrob(frame, grob, side = NULL,
          row = NULL, row.before = NULL, row.after = NULL,
          col = NULL, col.before = NULL, col.after = NULL,
          width = NULL, height = NULL,
          force.width = FALSE, force.height = FALSE, border = NULL,
          dynamic = FALSE)
}
\arguments{
  \item{gPath}{ A gPath object, which specifies a frame on the
    display list.}
  \item{frame}{ An object of class \code{frame}, typically the output
  from a call to \code{grid.frame}. }
  \item{grob}{ An object of class \code{grob}.  The object to be
    packed. }
  \item{redraw}{ A boolean indicating whether the output should be updated. }
  \item{side}{ One of \code{"left"}, \code{"top"}, \code{"right"},
    \code{"bottom"} to indicate which side to pack the object on. }
  \item{row}{ Which row to add the object to.  Must be between 1 and
    the-number-of-rows-currently-in-the-frame + 1, or \code{NULL} in
    which case the object occupies all rows. }
  \item{row.before}{ Add the object to a new row just before this row. }
  \item{row.after}{ Add the object to a new row just after this row. }
  \item{col}{ Which col to add the object to.  Must be between 1 and
    the-number-of-cols-currently-in-the-frame + 1, or \code{NULL} in
    which case the object occupies all cols.  }
  \item{col.before}{Add the object to a new col just before this col.}
  \item{col.after}{ Add the object to a new col just after this col. }
  \item{width}{ Specifies the width of the column that the object is
    added to (rather than allowing the width to be taken from the object). }
  \item{height}{ Specifies the height of the row that the object is
    added to (rather than allowing the height to be taken from the
    object). }
  \item{force.width}{ A logical value indicating whether the width of
    the column that the grob is being packed into should be EITHER the
    width specified in the call to \code{grid.pack} OR the maximum of
    that width and the pre-existing width. }
  \item{force.height}{ A logical value indicating whether the height of
    the column that the grob is being packed into should be EITHER the
    height specified in the call to \code{grid.pack} OR the maximum of
    that height and the pre-existing height. }
  \item{border}{ A \code{unit} object of length 4 indicating the borders
    around the object. }
  \item{dynamic}{ If the width/height is taken from the grob being
    packed, this boolean flag indicates whether the grobwidth/height
    unit refers directly to the grob, or uses a gPath to the grob.
    In the latter case, changes to the grob will trigger a
    recalculation of the width/height.}
}
\details{
  \code{packGrob} modifies the given frame grob and returns the modified
  frame grob.

  \code{grid.pack} destructively modifies a frame grob on the display
  list (and redraws the display list if \code{redraw} is \code{TRUE}).

  These are (meant to be) very flexible functions.  There are many
  different
  ways to specify where the new object is to be added relative to the
  objects already in the frame.  The function checks that the
  specification is not self-contradictory.

  NOTE that the width/height of the row/col that the object is added to
  is taken from the object itself unless the \code{width}/\code{height}
  is specified.
}
\value{
  \code{packGrob} returns a frame grob, but \code{grid.pack} returns
  \code{NULL}.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.frame}},
    \code{\link{grid.place}},
    \code{\link{grid.edit}}, and \code{\link{gPath}}.
}
\keyword{dplot}
