\name{rect}
\title{Draw One or More Rectangles}
\usage{
rect(xleft, ybottom, xright, ytop, density = NULL, angle = 45,
     col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"),
     \dots)
}
\alias{rect}
\arguments{
  \item{xleft}{a vector (or scalar) of left x positions.}
  \item{ybottom}{a vector (or scalar) of bottom y positions.}
  \item{xright}{a vector (or scalar) of right x positions.}
  \item{ytop}{a vector (or scalar) of top y positions.}
  \item{density}{the density of shading lines, in lines per inch.
    The default value of \code{NULL} means that no shading lines are drawn.
    A zero value of \code{density} means no shading lines whereas
    negative values (and \code{NA}) suppress shading (and so allow
    color filling).}
  \item{angle}{angle (in degrees) of the shading lines.}
  \item{col}{color(s) to fill or shade the rectangle(s) with.
    The default \code{NA} (or also \code{NULL}) means do not fill,
    i.e., draw transparent rectangles, unless \code{density} is specified.}
  \item{border}{color for rectangle border(s).  The default means
    \code{par("fg")}.  Use \code{border = NA} to omit borders.

    Can also be \code{FALSE} to suppress the border, or \code{TRUE} in
    which case \code{col} is used.}
  \item{lty}{line type for borders and shading; defaults to \code{"solid"}.}
  \item{lwd}{line width for borders and shading.}
  \item{\dots}{other graphical parameters such as \code{xpd}
    can be given as arguments.}
}
\description{
  \code{rect} draws a rectangle (or sequence of rectangles) with the
  given coordinates, fill and border colors.
}
\details{
  The positions supplied, i.e., \code{xleft, \dots},
  are relative to the current plotting region.  If the x-axis goes from
  100 to 200 then \code{xleft} must be larger than 100 and \code{xright}
  must be less than 200.

  It is a primitive function used in \code{\link{hist}},
  \code{\link{barplot}}, \code{\link{legend}}, etc.
}
\seealso{
  \code{\link{box}} for the \dQuote{standard} box around the plot;
  \code{\link{polygon}} and \code{\link{segments}} for flexible line
  drawing.

  \code{\link{par}} for how to specify colors.
}
\examples{
## set up the plot region:
op <- par(bg = "thistle")
plot(c(100, 250), c(300, 450), type = "n", xlab="", ylab="",
     main = "2 x 11 rectangles; 'rect(100+i,300+i,  150+i,380+i)'")
i <- 4*(0:10)
## draw rectangles with bottom left (100, 300)+i and top right (150, 380)+i
rect(100+i, 300+i, 150+i, 380+i, col=rainbow(11, start=.7,end=.1))
rect(240-i, 320+i, 250-i, 410+i, col=heat.colors(11), lwd=i/5)
## Background alternating  ( transparent / "bg" ) :
j <- 10*(0:5)
rect(125+j, 360+j,   141+j, 405+j/2, col = c(NA,0), border = "gold", lwd = 2)
rect(125+j, 296+j/2, 141+j, 331+j/5, col = c(NA,"midnightblue"))
mtext("+  2 x 6 rect(*, col = c(NA,0)) and  col = c(NA,\"m..blue\"))")

## an example showing colouring and shading
plot(c(100, 200), c(300, 450), type= "n", xlab="", ylab="")
rect(100, 300, 125, 350) # transparent
rect(100, 400, 125, 450, col="green", border="blue") # coloured
rect(115, 375, 150, 425, col=par("bg"), border="transparent")
rect(150, 300, 175, 350, density=10, border="red")
rect(150, 400, 175, 450, density=30, col="blue",
     angle=-30, border="transparent")

legend(180, 450, legend=1:4, fill=c(NA, "green", par("fg"), "blue"),
       density=c(NA, NA, 10, 30), angle=c(NA, NA, 30, -30))

par(op)
}
\keyword{aplot}
