% File src/library/grDevices/man/xy.coords.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{xy.coords}
\title{Extracting Plotting Structures}
\description{
  \code{xy.coords} is used by many functions to obtain
  x and y coordinates for plotting.  The use of this common mechanism
  across all relevant \R functions produces a measure of consistency.
}
\usage{
xy.coords(x, y = NULL, xlab = NULL, ylab = NULL, log = NULL,
          recycle = FALSE, setLab = TRUE)
}
\alias{xy.coords}
\arguments{
  \item{x, y}{the x and y coordinates of a set of points.
    Alternatively, a single argument \code{x} can be provided.}
  \item{xlab, ylab}{names for the x and y variables to be extracted.}
  \item{log}{character, \code{"x"}, \code{"y"} or both, as for
    \code{\link{plot}}.  Sets negative values to \code{\link{NA}} and
    gives a warning.}
  \item{recycle}{logical; if \code{TRUE}, recycle (\code{\link{rep}})
    the shorter of \code{x} or \code{y} if their lengths differ.}
  \item{setLab}{logical indicating if the resulting \code{xlab} and
    \code{ylab} should be constructed from the \dQuote{kind} of \code{(x,y)};
    otherwise, the arguments \code{xlab} and \code{ylab} are used.}
}
\details{
  An attempt is made to interpret the arguments \code{x} and \code{y} in
  a way suitable for bivariate plotting (or other bivariate procedures).

  If \code{y} is \code{NULL} and \code{x} is a
  \describe{
    \item{formula:}{of the form \code{yvar ~ xvar}. \code{xvar} and
      \code{yvar} are used as x and y variables.}

    \item{list:}{containing components \code{x} and \code{y}, these are
      used to define plotting coordinates.}

    \item{time series:}{the x values are taken to be
      \code{\link{time}(x)} and the y values to be the time series.}

    \item{matrix or \code{\link{data.frame}} with two or more
      columns:}{the first is assumed to contain the x values and the
      second the y values.  \emph{Note} that is also true if \code{x}
      has columns named \code{"x"} and \code{"y"}; these names will be
      irrelevant here.}
  }
  In any other case, the \code{x} argument is coerced to a vector and
  returned as \bold{y} component where the resulting \code{x} is just
  the index vector \code{1:n}.  In this case, the resulting \code{xlab}
  component is set to \code{"Index"} (if \code{setLab} is true as by default).

  If \code{x} (after transformation as above) inherits from class
  \code{"POSIXt"} it is coerced to class \code{"POSIXct"}.
}
\value{
  A list with the components
  \item{x}{numeric (i.e., \code{"double"}) vector of abscissa values.}
  \item{y}{numeric vector of the same length as \code{x}.}
  \item{xlab}{\code{character(1)} or \code{NULL}, the \sQuote{label} of
    \code{x}.}
  \item{ylab}{\code{character(1)} or \code{NULL}, the \sQuote{label} of
    \code{y}.}
}
\seealso{
  \code{\link{plot.default}}, \code{\link{lines}}, \code{\link{points}}
  and \code{\link{lowess}} are examples of functions which use this mechanism.
}
\examples{
ff <- stats::fft(1:9)
xy.coords(ff)
xy.coords(ff, xlab = "fft") # labels "Re(fft)",  "Im(fft)"
\dontshow{
stopifnot(identical(xy.coords(ff, xlab = "fft"),
                    xy.coords(ff, ylab = "fft")))
xy.labs <- function(...) xy.coords(...)[c("xlab","ylab")]
stopifnot(identical(xy.labs(ff, xlab = "fft", setLab = FALSE),
                    list(xlab = "fft", ylab = "fft")),
          identical(xy.labs(ff, ylab = "fft", setLab = FALSE),
                    list(xlab = NULL, ylab = "fft")),
          identical(xy.labs(ff, xlab = "Re(fft)", ylab = "Im(fft)", setLab = FALSE),
                    list(xlab = "Re(fft)", ylab = "Im(fft)")))
}% dont
with(cars, xy.coords(dist ~ speed, NULL)$xlab ) # = "speed"

xy.coords(1:3, 1:2, recycle = TRUE) # otherwise error "lengths differ"
xy.coords(-2:10, log = "y")
##> xlab: "Index"  \\\  warning: 3 y values <= 0 omitted ..
}
\keyword{dplot}
