\name{grid.place}
\alias{grid.place}
\title{ Place an Object within a Frame }
\description{
  This function provides a simpler interface to the \code{grid.pack()}
  function.  This can be used to place objects within the existing
  rows and columns of a frame layout.  You lose the ability to
  add new rows and columns and you lose the ability to affect the
  heights and widths of the rows and columns, \emph{but} you avoid
  some of the speed penalty of dealing with frames without having
  to specify a complicated combination of arguments to \code{grid.pack}.
}
\usage{
grid.place(frame, grob, grob.name="", draw=TRUE, row=1, col=1)
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
  \item{row}{ Which row to add the object to.  Must be between 1 and
    the-number-of-rows-currently-in-the-frame. }
  \item{col}{ Which col to add the object to.  Must be between 1 and
    the-number-of-cols-currently-in-the-frame.  }
}
\author{ Paul Murrell }
\seealso{
  \code{\link{grid.frame}} and
  \code{\link{grid.pack}}
}
\keyword{dplot}
