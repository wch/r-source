\name{xy.coords}
\title{Extracting Plotting Structures}
\usage{
xy.coords(x, y, xlab=NULL, ylab=NULL)
}
\alias{xy.coords}
\arguments{
\item{x,y}{the x and y coordinates of a set of points.
Alternatively, a single argument \code{x} can be be provided.
In this case, an attempt is made to interpret the
argument in a way suitable for plotting.
If the argument is a formula  \code{yvar ~ xvar}, \code{xvar} and \code{yvar}
are used as x and y variables;
if the argument is a list containing components \code{x} and
\code{y}, these are used are assumed to define plotting coordinates;
if the argument contains a time series, the x values are
taken to be time and the y values to be the time series;
if the argument is a matrix with two columns, the first
is assumed to contain the x values and the second the
y values; in any other case, the argument is coerced to a vector
and the values plotted against their indices.}
\item{xlab,ylab}{names for the x and y variables to be extracted.}
}
\description{
\code{xy.coords} is used by many function to obtain
x and y coordinates for plotting.
The use of this common mechanism across all \R functions
produces a measure of consistency.

\code{plot.default} and \code{\link{lowess}} are examples of functions
which use this mechanism.
}
\value{
A list with the components
\item{x}{numeric (i.e. \code{"double"}) vector of abscissa values.}
\item{y}{numeric vector of the same length as \code{x}.}
\item{xlab}{\code{character(1)} or \code{NULL}, the `label' of \code{x}.}
\item{ylab}{\code{character(1)} or \code{NULL}, the `label' of \code{y}.}
}
\examples{
xy.coords(fft(c(1:10)), NULL)
data(cars) ; attach(cars)
xy.coords(dist ~ speed, NULL)$xlab # = "speed"
}
\keyword{dplot}
