\name{units}
\title{Graphical Units}
\usage{
xinch(x=1)
yinch(y=1)
xyinch(xy=1)
}
\arguments{
  \item{x,y}{numeric vector}}
  \item{xy}{numeric of length 1 or 2.}
}
\alias{xinch}
\alias{yinch}
\alias{xyinch}
\description{
\code{xinch} and \code{yinch} convert the specified number of
inches given as their arguments into the correct units
for plotting with graphics functions.
\code{xyinch} does the same for a pair of numbers \code{xy}, simultaneously.
}
\examples{
all(c(xinch(),yinch()) == xyinch()) # TRUE
xyinch()
xyinch #- to see that is really   delta{"usr"} / "pin"

## plot labels offset 0.12 inches to the right
# of plotted symbols in a plot
data(mtcars)
attach(mtcars)
plot(mpg, disp, pch=19, main= "Motor Trend Cars")
text(mpg + xinch(0.12), disp, rownames(mtcars),adj=0, cex = .7, col='blue')
detach(mtcars)
}
\keyword{dplot}
