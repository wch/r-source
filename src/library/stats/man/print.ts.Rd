% File src/library/stats/man/print.ts.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{print.ts}
\title{Printing and Formatting of Time-Series Objects}
%\alias{format.ts}
\alias{.preformat.ts}
\alias{print.ts}
\description{
  Notably for calendar related time series objects,  
  \code{\link{format}} and \code{\link{print}} methods showing years,
  months and or quarters respectively.
}
\usage{
\method{print}{ts}(x, calendar, \dots)
.preformat.ts(x, calendar, \dots)
}
\arguments{
  \item{x}{a time series object.}
  \item{calendar}{enable/disable the display of information about
    month names, quarter names or year when printing.  The default is
    \code{TRUE} for a frequency of 4 or 12, \code{FALSE} otherwise.}
  \item{\dots}{additional arguments to \code{\link{print}} (or
    \code{\link{format}} methods).}
}
\details{
  The \code{\link{print}} method for \code{"ts"} objects prints a
  header (basically of \code{\link{tsp}(x)}), if \code{calendar} is
  false, and then prints the result of \code{.preformat.ts(x, *)}, which
  is typically a \code{\link{matrix}} with \code{\link{rownames}} built
  from the calendar times where applicable.
}
\seealso{
  \code{\link{print}},
  \code{\link{ts}}.
}
\examples{
print(ts(1:10, frequency = 7, start = c(12, 2)), calendar = TRUE)

print(sunsp.1 <- window(sunspot.month, end=c(1756, 12)))
m <- .preformat.ts(sunsp.1) # a character matrix
}
\keyword{ts}
