\name{grid.pack}
\alias{grid.pack}
\title{ Pack an Object within a Frame }
\description{
  This function, together with \code{grid.frame} is part of a
  GUI-builder-like interface to constructing graphical images.
  The idea is that you create a frame with \code{grid.frame} then
  use this function to pack objects into the frame.
}
\usage{
grid.pack(frame, grob, grob.name="", draw=TRUE, side=NULL,
          row=NULL, row.before=NULL, row.after=NULL,
          col=NULL, col.before=NULL, col.after=NULL,
          width=NULL, height=NULL,
          force.width=FALSE, force.height=FALSE, border=NULL)
}
\arguments{
  \item{frame}{ An object of class \code{frame}, typically the output
  from a call to \code{grid.frame}. }
  \item{grob}{ An object of class \code{grob}.  The object to be
    packed. }
  \item{grob.name}{ The name of the grob within the frame.  This is
    crucial if you intend to access the object again, for example, to
    edit it.}
  \item{draw}{ A boolean indicating whether the output should be updated. }
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
}
\details{
  This is (meant to be) a very flexible function.  There are many
  different
  ways to specify where the new object is to be added relative to the
  objects already in the frame.  The function checks that the
  specification is not self-contradictory.

  NOTE that the width/height of the row/col that the object is added to
  is taken from the object itself unless the \code{width}/\code{height}
  is specified.
}
\value{
  None.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.frame}}
}
\keyword{dplot}
