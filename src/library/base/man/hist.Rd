\name{hist}
\title{Histograms}
\usage{
hist(x, breaks, freq=TRUE, col=NULL, border=par("fg"), \dots)
}
\alias{hist}
\arguments{
\item{x}{a vector of values for which the histogram is desired.}
\item{breaks}{either a single number giving the
approximate number of cells for the histogram
or a vector giving the breakpoints between histogram cells.}
\item{freq}{a logical vector which indicates whether the
histogram is to present a representation of frequencies
(the default) or relative frequencies.}
\item{col}{a colour to be used to fill draw the bars.
The default of \code{NULL} yields unfilled bars.}
\item{border}{the color of the border around the bars.}
\item{\dots}{graphical parameters can also be specified as arguments.}
}
\description{
\code{hist} computes and plots a histogram of the given data values.
The histogram cells are intervals of the form
\code{[a,b)}, i.e. they include their left-hand endpoint,
but not their right one.
}
\examples{
data(islands)
hist(islands)
hist(islands, col="gray")
hist(islands, col="gray", border="gray")
}
