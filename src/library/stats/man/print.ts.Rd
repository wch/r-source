% File src/library/stats/man/print.ts.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{print.ts}
\title{Printing Time-Series Objects}
\alias{print.ts}
\description{
  Print method for time series objects.
}
\usage{
\method{print}{ts}(x, calendar, \dots)
}
\arguments{
  \item{x}{a time series object.}
  \item{calendar}{enable/disable the display of information about
    month names, quarter names or year when printing.  The default is
    \code{TRUE} for a frequency of 4 or 12, \code{FALSE} otherwise.}
  \item{\dots}{additional arguments to \code{\link{print}}.}
}
\details{
  This is the \code{\link{print}} methods for objects inheriting from
  class \code{"ts"}.
}
\seealso{
  \code{\link{print}},
  \code{\link{ts}}.
}
\examples{
print(ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)
}
\keyword{ts}
