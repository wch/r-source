% File src/library/graphics/man/axis.POSIXct.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later


\name{axis.POSIXct}
\alias{axis.POSIXct}
\alias{axis.Date}
\title{Date and Date-time Plotting Functions}
\description{
  Add a date/time axis to the current plot of an object of class
  \code{"POSIXt"} or \code{"Date"}, respectively.
}
\usage{
axis.POSIXct(side, x, at, format, labels = TRUE, \dots)
axis.Date(side, x, at, format, labels = TRUE, \dots)
}
\arguments{
  \item{side}{see \code{\link{axis}}.}
  \item{x, at}{optional \link{date-time} or \link{Date} objects,
    or other types of objects that can be converted appropriately.}
  \item{format}{an optional character string specifying the label format,
    see \code{\link{strftime}}.}
  \item{labels}{either a logical value specifying whether
    annotations are to be made at the tickmarks, or a character vector of
    labels to be placed at the tick points specified by \code{at}.}
  \item{\dots}{further arguments to be passed from or to other methods,
    typically \link{graphical parameters}.}
}
\details{
  If \code{at} is unspecified,
  \code{axis.POSIXct} and \code{axis.Date} work quite hard 
  (from \R 4.3.0 via \code{\link[grDevices:pretty.Date]{pretty}} for
  \link{date-time} classes) to choose
  suitable time units (years, months, days, hours, minutes, or seconds)
  and a sensible label \code{format} based on the axis range.
  \code{\link{par}("lab")} controls the approximate number of intervals.

  If \code{at} is supplied it specifies the locations of the ticks
  and labels. If the label \code{format} is unspecified, a good guess
  is made by looking at the granularity of \code{at}.
  Printing of tick labels can be suppressed with
  \code{labels = FALSE}.

  The date-times for a \code{"POSIXct"} input are interpreted in the
  time zone give by the \code{"tzone"} attribute if there is one,
  otherwise the current time zone.

  The way the date-times are rendered (especially month names) is
  controlled by the locale setting of category \code{"LC_TIME"} (see
  \code{\link{Sys.setlocale}}).
}
\value{
  The locations on the axis scale at which tick marks were drawn.
}
\seealso{
  \link{DateTimeClasses}, \link{Dates} for details of the classes.

  \code{\link{Axis}}.
}
\examples{
with(beaver1, {
    opar <- par(mfrow = c(3,1))
    time <- strptime(paste(1990, day, time \%/\% 100, time \%\% 100),
                     "\%Y \%j \%H \%M")
    plot(time, temp, type = "l") # axis at 6-hour intervals
    # request more ticks
    olab <- par(lab = c(10, 10, 7))
    plot(time, temp, type = "l")
    par(olab)
    # now label every hour on the time axis
    plot(time, temp, type = "l", xaxt = "n")
    r <- as.POSIXct(round(range(time), "hours"))
    axis.POSIXct(1, at = seq(r[1], r[2], by = "hour"), format = "\%H")
    par(opar) # reset changed par settings
})

plot(.leap.seconds, seq_along(.leap.seconds), type = "n", yaxt = "n",
     xlab = "leap seconds", ylab = "", bty = "n")
rug(.leap.seconds)
## or as dates
lps <- as.Date(.leap.seconds)
plot(lps, seq_along(.leap.seconds),
     type = "n", yaxt = "n", xlab = "leap seconds",
     ylab = "", bty = "n")
rug(lps)

## 100 random dates in a 10-week period
random.dates <- as.Date("2001/1/1") + 70*sort(stats::runif(100))
plot(random.dates, 1:100)
# or for a better axis labelling
plot(random.dates, 1:100, xaxt = "n")
axis.Date(1, at = seq(as.Date("2001/1/1"), max(random.dates)+6, "weeks"))
axis.Date(1, at = seq(as.Date("2001/1/1"), max(random.dates)+6, "days"),
     labels = FALSE, tcl = -0.2)

## axis.Date() with various data types:
x <- seq(as.Date("2022-01-20"), as.Date("2023-03-21"), by = "days")
plot(data.frame(x, y = 1), xaxt = "n")
legend("topleft", title = "input",
       legend = c("character", "Date", "POSIXct", "POSIXlt", "numeric"),
       fill = c("violet", "red", "orange", "coral1", "darkgreen"))
axis.Date(1)
axis.Date(3, at = "2022-04-01", col.axis = "violet")
axis.Date(3, at = as.Date("2022-07-01"), col.axis = "red")
axis.Date(3, at = as.POSIXct(as.Date("2022-10-01")), col.axis = "orange")
axis.Date(3, at = as.POSIXlt(as.Date("2023-01-01")), col.axis = "coral1")
axis.Date(3, at = as.integer(as.Date("2023-04-01")), col.axis = "darkgreen")
## automatically extends the format:
axis.Date(1, at = "2022-02-15", col.axis = "violet",
         col = "violet", tck = -0.05, mgp = c(3,2,0))

## axis.POSIXct() with various data types (2 minutes):
x <- as.POSIXct("2022-10-01") + c(0, 60, 120)
attributes(x)   # no timezone
plot(data.frame(x, y = 1), xaxt = "n")
legend("topleft", title = "input",
       legend = c("character", "Date", "POSIXct", "POSIXlt", "numeric"),
       fill = c("violet", "red", "orange", "coral1", "darkgreen"))
axis.POSIXct(1)
axis.POSIXct(3, at = "2022-10-01 00:01", col.axis = "violet")
axis.POSIXct(3, at = as.Date("2022-10-01"), col.axis = "red")
axis.POSIXct(3, at = as.POSIXct("2022-10-01 00:01:30"), col.axis = "orange")
axis.POSIXct(3, at = as.POSIXlt("2022-10-01 00:02"), col.axis = "coral1")
axis.POSIXct(3, at = as.numeric(as.POSIXct("2022-10-01 00:00:30")),
                col.axis = "darkgreen")
## automatically extends format (here: subseconds):
axis.POSIXct(3, at = as.numeric(as.POSIXct("2022-10-01 00:00:30")) + 0.25,
                col.axis = "forestgreen", col = "darkgreen", mgp = c(3,2,0))

## axis.POSIXct: 2 time zones
HST <- as.POSIXct("2022-10-01", tz = "HST") + c(0, 60, 60*60)
CET <- HST
attr(CET, "tzone") <- "CET"
plot(data.frame(HST, y = 1), xaxt = "n", xlab = "Hawaii Standard Time (HST)")
axis.POSIXct(1, HST)
axis.POSIXct(1, HST, at = "2022-10-01 00:10", col.axis = "violet")
axis.POSIXct(3, CET)
mtext(3, text = "Central European Time (CET)", line = 3)
axis.POSIXct(3, CET, at="2022-10-01 12:10", col.axis = "violet")
}
\keyword{utilities}
\keyword{chron}
