\name{rect}
\title{Draw a Rectangle}
\usage{
rect(xleft, ybottom, xright, ytop,
     col=NULL, border=par("fg"), lty=NULL, lwd=par("lwd"), xpd=FALSE)
}
\alias{rect}
\arguments{
  \item{xleft}{a vector (or scalar) of left x positions.}
  \item{ybottom}{a vector (or scalar) of bottom y positions.}
  \item{xright}{a vector (or scalar) of right x positions.}
  \item{ytop}{a vector (or scalar) of top y positions.}
  \item{col}{color(s) to fill the rectangle(s) with.}
  \item{border}{color for rectangle border(s).}
  \item{lty}{line type for borders; defaults to \code{"solid"}.}
  \item{lwd}{width for borders.}
  \item{xpd}{logical (\emph{``expand''}); if \code{FALSE}, everything is
    clipped to the plot region.}
}
\description{
  \code{rect} draws a rectangle (or sequence of rectangles) with the
  given coordinates.  It is a primitive function used in
  \code{\link{hist}}.
}
\details{
  The positions supplied, i.e., \code{xleft, \dots},
  are relative to the current plotting region.  If the x-axis goes from
  100 to 200 then \code{xleft} must be larger than 100 and \code{xright}
  must be less than 200.
}
\seealso{
  \code{\link{box}} for the ``standard'' box around the plot;
  \code{\link{polygon}} and \code{\link{segments}} for flexible line
  drawing.
}
\examples{
## set up the plot region:
plot(c(100, 250), c(300, 450), type = "n",
     main = "2 x 11 rectangles; `rect(100+i,300+i,  150+i,380+i)'")
i <- 4*(0:10)
## draw rectangles with bottom left (100, 300)+i  and top right (150, 380)+i
rect(100+i, 300+i, 150+i, 380+i, col=rainbow(11, start=.7,end=.1))
rect(240-i, 320+i, 250-i, 410+i, col=heat.colors(11), lwd=i/5)
}
\keyword{aplot}
