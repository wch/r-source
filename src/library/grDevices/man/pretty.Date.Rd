% File src/library/grDevices/man/pretty.Date.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2010-2016 R Core Team
% Distributed under GPL 2 or later

\name{pretty.Date}
\title{Pretty Breakpoints for Date-Time Classes}
\alias{pretty.Date}%- keep alias{}es - such that ?pretty.Date  works
\alias{pretty.POSIXt}
\usage{
\method{pretty}{Date}(x, n = 5, min.n = n \%/\% 2, sep = " ", \dots)
\method{pretty}{POSIXt}(x, n = 5, min.n = n \%/\% 2, sep = " ", \dots)
}
\arguments{
  \item{x}{an object of class \code{"Date"} or \code{"POSIXt"} (i.e.,
    \code{"POSIXct"} or \code{"POSIXlt"}). }
  \item{n}{integer giving the \emph{desired} number of intervals.}
  \item{min.n}{nonnegative integer giving the \emph{minimal} number of
    intervals.}
  \item{sep}{character string, serving as a separator for certain
    formats (e.g., between month and year). }
  \item{\dots}{further arguments for compatibility with the generic,
    ignored.}
}
\description{
  Compute a sequence of about \code{n+1} equally spaced \sQuote{nice}
  values which cover the range of the values in \code{x}, possibly of
  length one, when \code{min.n = 0} and there is only one unique
  \code{x}.
}
%% \note{The current algorithm is geared towards small \code{n}, not
%% larger than say, 50 or 100.
%% }
\value{
  A vector (of the suitable class) of locations, with attribute
  \code{"labels"} giving corresponding formatted character labels.
}
\seealso{
  \code{\link{pretty}} for the default method.
}
\examples{
\donttest{% time-dependent
pretty(Sys.Date())
pretty(Sys.time(), n = 10)
}
pretty(as.Date("2000-03-01")) # R 1.0.0 came in a leap year

## time ranges in diverse scales:% also in ../../../../tests/reg-tests-1c.R
require(stats)
steps <- setNames(,
    c("10 secs", "1 min", "5 mins", "30 mins", "6 hours", "12 hours",
      "1 DSTday", "2 weeks", "1 month", "6 months", "1 year",
      "10 years", "50 years", "1000 years"))
x <- as.POSIXct("2002-02-02 02:02")
lapply(steps,
       function(s) {
           at <- pretty(seq(x, by = s, length = 2), n = 5)
           attr(at, "labels")
       })
}
\keyword{dplot}
