\name{bxp}
\title{Box Plots from Summaries}
\usage{
bxp(z, width, varwidth=FALSE, notch=FALSE, \dots)
}
\alias{bxp}
\arguments{
\item{z}{a list containing data summaries to be used in constructing the plots.
  These are usually the result of a call to \code{\link{boxplot}},
  but can be generated in any fashion.}
\item{width}{a vector giving the relative widths of the boxes making up
  the plot.}
\item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are drawn
  with widths proportional to the square-roots of the number of
  observations in the groups.}
\item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in each
  side of the boxes.  If the notches of two plots do not
  overlap then the medians are significantly different at the
  5 percent level.}
\item{\dots}{Graphical parameters can be passed as arguments to this function.}
}
\description{
 \code{bxp(..)} draws box plots based on the given summaries in \code{z}.
  It is usually called from within \code{\link{boxplot}(..)}, but can be invoked
  directly.
}
\value{An invisible vector with the x-coordinates of box centers, useful
  for adding to the plot.
}
\examples{
str(bx.p <- boxplot(split(rnorm(100), gl(5,20)), plot = FALSE))
op <- par(mfrow= c(2,2))
bxp(bx.p)
bxp(bx.p, notch = TRUE)
bxp(bx.p, notch = TRUE, col= 'lightblue')
bxp(bx.p, notch = TRUE, col= 'lightblue', border='red', log = 'x')
par(op)
}
\keyword{aplot}
