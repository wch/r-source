% File src/library/grid/man/grid.place.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{grid.place}
\alias{grid.place}
\alias{placeGrob}
\title{ Place an Object within a Frame }
\description{
  These functions provide a simpler (and faster) alternative
  to the \code{grid.pack()} and \code{packGrob}
  functions.  They can be used to place objects within the existing
  rows and columns of a frame layout.  They do not provide the ability to
  add new rows and columns nor do they affect the
  heights and widths of the rows and columns.
}
\usage{
grid.place(gPath, grob, row = 1, col = 1, redraw = TRUE)
placeGrob(frame, grob, row = NULL, col = NULL)
}
\arguments{
  \item{gPath}{ A gPath object, which specifies a frame on the
    display list.}
  \item{frame}{ An object of class \code{frame}, typically the output
  from a call to \code{grid.frame}. }
  \item{grob}{ An object of class \code{grob}.  The object to be
    placed. }
  \item{row}{ Which row to add the object to.  Must be between 1 and
    the-number-of-rows-currently-in-the-frame. }
  \item{col}{ Which col to add the object to.  Must be between 1 and
    the-number-of-cols-currently-in-the-frame.  }
  \item{redraw}{ A boolean indicating whether the output should be updated. }
}
\details{
  \code{placeGrob} modifies the given frame grob and returns the modified
  frame grob.

  \code{grid.place} destructively modifies a frame grob on the display
  list (and redraws the display list if \code{redraw} is \code{TRUE}).

}
\value{
  \code{placeGrob} returns a frame grob, but \code{grid.place} returns
  \code{NULL}.
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.frame}},
  \code{\link{grid.pack}},
  \code{\link{grid.edit}}, and \code{\link{gPath}}.
}
\keyword{dplot}
