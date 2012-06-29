% File src/library/graphics/man/hist.POSIXt.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{hist.POSIXt}
\alias{hist.POSIXt}
\alias{hist.Date}
\title{Histogram of a Date or Date-Time Object}
\description{
  Method for \code{\link{hist}} applied to date or date-time objects.
}
\usage{
\method{hist}{POSIXt}(x, breaks, \dots,
     xlab = deparse(substitute(x)),
     plot = TRUE, freq = FALSE,
     start.on.monday = TRUE, format)

\method{hist}{Date}(x, breaks, \dots,
     xlab = deparse(substitute(x)),
     plot = TRUE, freq = FALSE,
     start.on.monday = TRUE, format)
}
\arguments{
  \item{x}{an object inheriting from class \code{"POSIXt"} or \code{"Date"}.}
  \item{breaks}{
    a vector of cut points \emph{or} number giving the number of
    intervals which \code{x} is to be cut into \emph{or} an
    interval specification, one of \code{"days"}, \code{"weeks"},
    \code{"months"}, \code{"quarters"} or \code{"years"},
    plus \code{"secs"}, \code{"mins"}, \code{"hours"} for
    date-time objects.
 }
  \item{\dots}{\link{graphical parameters}, or arguments to
    \code{\link{hist.default}} such as \code{include.lowest},
      \code{right} and \code{labels}.}
  \item{xlab}{a character string giving the label for the x axis, if plotted.}
  \item{plot}{logical.  If \code{TRUE} (default), a histogram is
    plotted, otherwise a list of breaks and counts is returned.}
  \item{freq}{logical; if \code{TRUE}, the
    histogram graphic is a representation of frequencies, i.e,
    the \code{counts} component of the result; if \code{FALSE},
    \emph{relative} frequencies (probabilities) are plotted.}
  \item{start.on.monday}{logical. If \code{breaks = "weeks"}, should the
    week start on Mondays or Sundays?}
  \item{format}{for the x-axis labels. See \code{\link{strptime}}.}
}
\details{
  Note that unlike the default method, \code{breaks} is a required argument.

  Using \code{breaks = "quarters"} will create intervals of 3 calendar
  months, with the intervals beginning on January 1, April 1,
  July 1 or October 1, based upon \code{min(x)} as appropriate.
}
\value{
  An object of class \code{"histogram"}: see \code{\link{hist}}.
}
\seealso{
  \code{\link{seq.POSIXt}}, \code{\link{axis.POSIXct}}, \code{\link{hist}}
}
\examples{
hist(.leap.seconds, "years", freq = TRUE)
hist(.leap.seconds,
     seq(ISOdate(1970, 1, 1), ISOdate(2015, 1, 1), "5 years"))

## 100 random dates in a 10-week period
random.dates <- as.Date("2001/1/1") + 70*stats::runif(100)
hist(random.dates, "weeks", format = "\%d \%b")
}
\keyword{chron}
\keyword{dplot}
\keyword{hplot}
