% File src/library/base/man/Dates.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{Dates}
\alias{Date}
\alias{Dates}
\alias{print.Date}
\alias{summary.Date}
\alias{Math.Date}
\alias{Summary.Date}
\alias{[.Date}
\alias{[<-.Date}
\alias{length<-.Date}
\alias{[[.Date}
\alias{as.data.frame.Date}
\alias{as.list.Date}
\alias{c.Date}
\alias{mean.Date}
\alias{split.Date}

\title{Date Class}
\description{
  Description of the class \code{"Date"} representing calendar dates.
}
\usage{
\method{summary}{Date}(object, digits = 12, \dots)

\method{print}{Date}(x, max = NULL, \dots)
% not sure if we should not rather _drop_ the 'recursive'!
%\method{c}{Date}(\dots, recursive = FALSE)
}
\arguments{
  \item{object, x}{a \code{Date} object to be summarized or printed.}
  \item{digits}{number of significant digits for the computations.}
  \item{max}{numeric or \code{NULL}, specifying the maximal number of
    entries to be printed.  By default, when \code{NULL},
    \code{\link{getOption}("max.print")} used.}
  \item{\dots}{further arguments to be passed from or to other methods.}
  %% \item{\dots, recursive}{further arguments to be passed from or to
  %%   other methods; \code{recursive} is not considered in \code{Date} method.}
}
\details{
  Dates are represented as the number of days since 1970-01-01, with
  negative values for earlier dates.  They are always printed
  following the rules of the current Gregorian calendar, even though
  that calendar was not in use long ago (it was adopted in 1752 in
  Great Britain and its colonies).

  It is intended that the date should be an integer, but this is not
  enforced in the internal representation.  Fractional days will be
  ignored when printing.  It is possible to produce fractional days via
  the \code{mean} method or by adding or subtracting (see
  \code{\link{Ops.Date}}).

  From the many methods, see \code{methods(class = "Date")}, a few are
  documented separately, see below.
}
\seealso{
  \code{\link{Sys.Date}} for the current date.

  \code{\link{weekdays}} for convenience extraction functions.

  Methods with extra arguments and documentation:
  \describe{
    \item{\code{\link{Ops.Date}}}{for operators on \code{"Date"} objects.}

    \item{\code{\link{format.Date}}}{for conversion to and from character strings.}

    \item{\code{\link{axis.Date}}}{and \code{\link{hist.Date}} for plotting.}

    \item{\code{\link{seq.Date}}}{, \code{\link{cut.Date}}, and
      \code{\link{round.Date}} for utility operations.}
  }
  \code{\link{DateTimeClasses}} for date-time classes.
}
\examples{\donttest{
(today <- Sys.Date())
format(today, "\%d \%b \%Y")  # with month as a word
(tenweeks <- seq(today, length.out=10, by="1 week")) # next ten weeks
weekdays(today)
months(tenweeks)
}% dont
(Dls <- as.Date(.leap.seconds))

##  length(<Date>) <- n   now works
ls <- Dls; length(ls) <- 12
l2 <- Dls; length(l2) <- 5 + length(Dls)
stopifnot(exprs = {
  ## length(.) <- * is compatible to subsetting/indexing:
  identical(ls, Dls[seq_along(ls)])
  identical(l2, Dls[seq_along(l2)])
  ## has filled with NA's
  is.na(l2[(length(Dls)+1):length(l2)])
})
}
\keyword{utilities}
\keyword{chron}
