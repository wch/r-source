\name{units}
\title{Graphical Units}
\usage{
xinch(x=1, warn.log=TRUE)
yinch(y=1, warn.log=TRUE)
xyinch(xy=1, warn.log=TRUE)
cm(x)
}
\arguments{
  \item{x,y}{numeric vector}
  \item{xy}{numeric of length 1 or 2.}
  \item{warn.log}{logical; if \code{TRUE}, a warning is printed in case
    of active log scale.}
}
\alias{xinch}
\alias{yinch}
\alias{xyinch}
\alias{cm}
\description{
\code{xinch} and \code{yinch} convert the specified number of
inches given as their arguments into the correct units
for plotting with graphics functions.
Usually, this only makes sense when normal coordinates are used, i.e.,
\emph{no} \code{log} scale (see \code{\link{par}(log=..)}).

\code{xyinch} does the same for a pair of numbers \code{xy}, simultaneously.

\code{cm} translates inches in to cm (centimeters).
}
\examples{
all(c(xinch(),yinch()) == xyinch()) # TRUE
xyinch()
xyinch #- to see that is really   delta{"usr"} / "pin"

cm(1)# = 2.54

## plot labels offset 0.12 inches to the right
## of plotted symbols in a plot
data(mtcars)
attach(mtcars)
plot(mpg, disp, pch=19, main= "Motor Trend Cars")
text(mpg + xinch(0.12), disp, rownames(mtcars),adj=0, cex = .7, col='blue')
detach(mtcars)
}
\keyword{dplot}
