\name{axis.POSIXct}
\alias{axis.POSIXct}
\alias{plot.POSIXct}
\alias{plot.POSIXlt}
\title{Date-time Plotting Functions}
\description{
  Functions to manipulate objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times (to the nearest
  second). 
}
\usage{
plot.POSIXct(x, y, xlab = "", ...)
plot.POSIXlt(x, y, xlab = "", ...)
axis.POSIXct(side, x, format, ...)
}
\arguments{
  \item{x}{A date-time object.}
  \item{y}{numeric values to be plotted against \code{x}.}
  \item{xlab}{a character string giving the label for the x axis.}
  \item{side}{See \code{\link{axis}}.}
  \item{format}{See \code{\link{strptime}}.}
  \item{\dots}{Further arguments to be passed from or to other methods.}
}
\details{
  The functions plot against an x-axis of date-times.
  \code{axis.POSIXct} works quite hard to choose suitable time units
  (years, months, days, hours, minutes or seconds) and a sensible
  output format, but this can be overridden by supplying a \code{format}
  specification.
}
\seealso{
  \link{DateTimeClasses} for details of the classes.
}
\examples{
\dontrun{library(MASS)
data(beav1)
attach(beav1)
time <- strptime(paste(day, time \%/\% 100, time \%\% 100),
                 "\%j \%H \%M")
plot(time, temp, type="l") # axis at 4-hour intervals.
detach("beav1")}

plot(.leap.seconds, 1:22, type="n", yaxt="n",
     xlab="leap seconds", ylab="")
rug(.leap.seconds)
}
\keyword{utilities}
\keyword{chron}
