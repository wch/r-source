\name{bxp}
\title{Box Plots from Summaries}
\usage{
bxp(z, width, varwidth=FALSE, notch=FALSE, \dots)
}
\alias{bxp}
\arguments{
\item{z}{a list containing data summaries to
be used in constructing the plots.
These are usually the result of a call to \code{boxplot},
but can be generated in any fashion.}
\item{width}{a vector giving the relative widths of the boxes
making up the plot.}
\item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes
are drawn with widths proportional to the square-roots
of the number of observations in the groups.}
\item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in each
side of the boxes.  If the notches of two plots do not
overlap then the medians are significantly different at the
5 percent level.}
\item{\dots}{Graphical parameters can be passed as arguments to this
function.}
}
\value{
This function is invoked for its side effect which is to
draw box plots based on the given summaries.
It is usually called from \code{boxplot}, abut can be invoked
directly.
}
\keyword{aplot}
