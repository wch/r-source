\name{units}
\title{Graphical Units}
\usage{
xinch(x=1)
yinch(x=1)
}
\alias{xinch}
\alias{yinch}
\description{
\code{xinch} and \code{yinch} convert the specified number of
inches given as their arguments into the correct units
for plotting with graphics functions.}
\examples{
# plot labels offset 0.2 inches to the right
# of plotted symbols in a plot
data(mtcars)
attach(mtcars)
plot(mpg, disp, pch=19)
text(mpg + xinch(0.12), disp, rownames(mtcars),adj=0, cex = .6, col='blue')
detach(mtcars)
}
\keyword{dplot}
