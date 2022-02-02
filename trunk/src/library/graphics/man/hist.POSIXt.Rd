% File src/library/graphics/man/hist.POSIXt.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{hist.POSIXt}
\alias{hist.POSIXt}
\alias{hist.Date}
\title{Histogram of a Date or Date-Time Object}
\description{
  Methods for \code{\link{hist}} applied to date (class \code{"\link{Date}"})
  or date-time (class  \code{"\link{POSIXt}"}) objects.
}
\usage{
\method{hist}{POSIXt}(x, breaks, \dots,
     xlab = deparse1(substitute(x)),
     plot = TRUE, freq = FALSE,
     start.on.monday = TRUE, format, right = TRUE)

\method{hist}{Date}(x, breaks, \dots,
     xlab = deparse1(substitute(x)),
     plot = TRUE, freq = FALSE,
     start.on.monday = TRUE, format, right = TRUE)
}
\arguments{
  \item{x}{an object inheriting from class \code{"\link{POSIXt}"}
    or \code{"\link{Date}"}.}
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
    \code{density} and \code{labels}.}
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
  \item{right}{logical; if \code{TRUE}, the histogram cells are
    right-closed (left open) intervals.}
}
\details{
  Note that unlike the default method, \code{breaks} is a required argument.

  Using \code{breaks = "quarters"} will create intervals of 3 calendar
  months, with the intervals beginning on January 1, April 1,
  July 1 or October 1, based upon \code{min(x)} as appropriate.

  With the default \code{right = TRUE}, breaks will be set
  on the last day of the previous period when \code{breaks} is
  \code{"months"}, \code{"quarters"} or \code{"years"}.  Use
  \code{right = FALSE} to set them to the first day of the
  interval shown in each bar.
}
\value{
  An object of class \code{"histogram"}: see \code{\link{hist}}.
}
\seealso{
  \code{\link{seq.POSIXt}}, \code{\link{axis.POSIXct}}, \code{\link{hist}}
}
\examples{
hist(.leap.seconds, "years", freq = TRUE)
brks <- seq(ISOdate(1970, 1, 1), ISOdate(2030, 1, 1), "5 years")
hist(.leap.seconds, brks)
rug(.leap.seconds, lwd=2)
## show that  'include.lowest' "works"
stopifnot(identical(c(2L, rep(1L,11)),
   hist(brks, brks, plot=FALSE, include.lowest=TRUE )$counts))
tools::assertError(verbose=TRUE, ##--> 'breaks' do not span range of 'x'
   hist(brks, brks, plot=FALSE, include.lowest=FALSE))
## The default fuzz in hist.default()  "kills" this, with a "wrong" message:
try ( hist(brks[-13] + 1, brks, include.lowest = FALSE) )
## and decreasing 'fuzz' solves the issue:
hb <- hist(brks[-13] + 1, brks, include.lowest = FALSE, fuzz = 1e-10)
stopifnot(hb$counts == 1)

## 100 random dates in a 10-week period
random.dates <- as.Date("2001/1/1") + 70*stats::runif(100)
hist(random.dates, "weeks", format = "\%d \%b")
}
\keyword{chron}
\keyword{dplot}
\keyword{hplot}
