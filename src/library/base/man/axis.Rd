\name{axis}
\alias{axis}
\title{Add an Axis to a Plot}
\description{Adds an axis to the current plot, allowing the
  specification of the side, position, labels, and other options.
}
\usage{
axis(side, at, labels = TRUE, \dots)
}
\arguments{
  \item{side}{an integer specifying which side of the plot the axis is
    to be drawn on.}
  \item{at}{the points at which tick-marks are to be drawn. Non-finite
    (infinite, \code{NaN} or \code{NA}) values are omitted.}
  \item{labels}{this can either be a logical value specifying whether
    (numerical) annotations are to be made at the tickmarks, or a vector
    of character strings to be placed at the tickpoints.}
  \item{\dots}{graphical parameters may also be passed as arguments to
    this function.}
}
\value{
  This function is invoked for its side effect, which is to add an axis
  to an already existing plot.  The axis is placed as follows: 1=below,
  2=left, 3=above and 4=right.
}
\details{
  The axis line is drawn from the lowest to the highest value of
  \code{at}, but will be clipped at the plot region. Only ticks which
  are drawn from points within the plot region (up to a tolerance for
  rounding error) are plotted, but the ticks and their labels may well
  extend outside the plot region.
}
\examples{
plot(1:4, rnorm(4), axes=FALSE)
axis(1, 1:4, LETTERS[1:4])
axis(2)
box() #- to make it look "as usual"

plot(1:7, rnorm(7), type = 's', xaxt='n', col = 'red')
axis(1, 1:7, LETTERS[1:7], col.axis = 'blue')
\testonly{ # test sorting of labels
Y <- c(10.50, 4.0, 13.75, 7.25)
plot(1:4, Y, xlim=c(0,5), ylim=c(0,15))
axis(side=4, at=Y, labels=LETTERS[1:4])
}
}
\keyword{aplot}
