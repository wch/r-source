\name{axis.POSIXct}
\alias{axis.POSIXct}
\alias{plot.POSIXct}
\alias{plot.POSIXlt}
\title{Date-time Plotting Functions}
\description{
  Functions to plot objects of classes \code{"POSIXlt"} and
  \code{"POSIXct"} representing calendar dates and times. 
}
\usage{
axis.POSIXct(side, x, at, format, \dots)

\method{plot}{POSIXct}(x, y, xlab = "", \dots)

\method{plot}{POSIXlt}(x, y, xlab = "", \dots)
}
\synopsis{
plot.POSIXct(x, y, xlab = "", axes = TRUE, frame.plot = axes, xaxt = par("xaxt"), ...)
plot.POSIXlt(x, y, xlab = "", axes = TRUE, frame.plot = axes, xaxt = par("xaxt"), ...)
axis.POSIXct(side, x, at, format, ...)
}
\arguments{
  \item{x, at}{A date-time object.}
  \item{y}{numeric values to be plotted against \code{x}.}
  \item{xlab}{a character string giving the label for the x axis.}
  \item{side}{See \code{\link{axis}}.}
  \item{format}{See \code{\link{strptime}}.}
  \item{\dots}{Further arguments to be passed from or to other methods,
    typically graphical parameters or arguments of
    \code{\link{plot.default}}.}
}
\details{
  The functions plot against an x-axis of date-times.
  \code{axis.POSIXct} works quite hard to choose suitable time units
  (years, months, days, hours, minutes or seconds) and a sensible
  output format, but this can be overridden by supplying a \code{format}
  specification.

  If \code{at} is supplied for \code{axis.POSIXct} it specifies the
  locations of the ticks and labels: if \code{x} is specified a
  suitable grid of labels is chosen. 
}
\seealso{
  \link{DateTimeClasses} for details of the classes.
}
\examples{
res <- try(data(beav1, package = MASS))
if(!inherits(res, "try-error")) {
attach(beav1)
time <- strptime(paste(1990, day, time \%/\% 100, time \%\% 100),
                 "\%Y \%j \%H \%M")
plot(time, temp, type="l") # axis at 4-hour intervals.
# now label every hour on the time axis
plot(time, temp, type="l", xaxt="n")
r <- as.POSIXct(round(range(time), "hours"))
axis.POSIXct(1, at=seq(r[1], r[2], by="hour"), format="\%H")
rm(time)
detach(beav1)
}

plot(.leap.seconds, 1:22, type="n", yaxt="n",
     xlab="leap seconds", ylab="", bty="n")
rug(.leap.seconds)
}
\keyword{utilities}
\keyword{chron}
