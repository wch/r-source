\name{axis}
\title{Add an Axis to a Plot}
\usage{
axis(side, at, labels=TRUE, \dots)
}
\alias{axis}
\arguments{
\item{side}{an integer specifying which side of the plot
the axis is to be drawn on.}
\item{at}{the points at which tick-marks are to be drawn.}
\item{labels}{this can either be a logical value specifying
whether (numerical) annotations are to be made at
the tickmarks,
or a vector of character strings to be placed
at the tickpoints.}
\item{\dots}{graphical parameters may also be passed as
aruments to this function.}
}
\value{
This function is invoked for its side effect,
which is to add an axis to an already existing plot.
The axis is placed as follows:
1=below, 2=left, 3=above and 4=right.
}
\examples{
plot(1:4, rnorm(4), axes=FALSE)
axis(1, 1:4, LETTERS[1:4])
axis(2)
box() #- to make it look "as usual"

##-- This still fails in older versions of R :
plot(1:7, rnorm(7), type = 's', xaxt='n', col = 'red')
axis(1, 1:7, LETTERS[1:7], col.axis = 'blue')
}
\keyword{aplot}
