\name{rect}
\title{Draw a Rectangle}
\usage{
rect(xleft, ybottom, xright, ytop, \dots)
}
\alias{rect}
\arguments{
  \item{xleft}{a vector (or scalar) of left x positions.}
  \item{ybottom}{a vector (or scalar) of bottom y positions.}
  \item{xright}{a vector (or scalar) of right x positions.}
  \item{ytop}{a vector (or scalar) of top y positions.}
  \item{\dots}{Further graphical parameters (see \code{\link{par}}) may
    also be supplied as arguments.}
}
\description{
  \code{rect} draws a rectangle (or sequence of rectangles) with the
  given coordinates.  It is a primitive function used in
  \code{\link{hist}}.  The value supplied, i.e., \code{xleft, \dots},
  are relative to the current plotting region. If the x-axis goes from
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
     main = "11 rectangles using `rect(120+i,300+i,  177+i,380+i)'")
i <- 4*(0:10)
## draw rectangles with bottom left (120, 300)+i  and top right (177, 380)+i
rect(120+i, 300+i, 177+i, 380+i, col=rainbow(11, start=.7,end=.1))
}
\keyword{aplot}
