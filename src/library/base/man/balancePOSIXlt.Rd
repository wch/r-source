% File src/library/base/man/balancePOSIXlt.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2022 R Core Team
% Distributed under GPL 2 or later
\name{balancePOSIXlt}
\title{Balancing \dQuote{Ragged} and Out-of-range POSIXlt Date-Times}
\alias{balancePOSIXlt}
% implementation mostly in ../R/datetime.R -- many more in ./DateTimeClasses.Rd
\usage{
balancePOSIXlt(x, fill.only = FALSE, classed = TRUE)
}
\arguments{
  \item{x}{an \R object inheriting from \code{"POSIXlt"}, see \code{\link{POSIXlt}}.}
  \item{fill.only}{a \code{\link{logical}} specifying if \code{balancePOSIXlt(x, ..)}
    should only \dQuote{fill up} by recycling, but not re-check validity
    nor recompute, e.g., \code{x$wday} and \code{x$yday}.}
  \item{classed}{a \code{\link{logical}} specifying if the result should be
    classed, true by default.  Using \code{balancePOSIXlt(x, classed = FALSE)}
    is equivalent to but faster than \code{unclass(balancePOSIXlt(x))}.}
}
\section{\dQuote{Ragged} and Out-of-range \emph{vs} \dQuote{Balanced} POSIXlt}{
  Note that \code{"POSIXlt"} objects \code{x} may have their (9 to 11) list
  components of different \code{\link{length}}, by simply recycling them to
  full length.  Previous to \R 4.3.0, this has worked in printing,
  formatting, and conversion to \code{"POSIXct"}, but often not for
  \code{length()}, conversion to \code{"Date"} or indexing, i.e.,
  subsetting, \code{\link{[}}, or subassigning, \code{\link{[<-}}.

  Relatedly, components \code{sec}, \code{min}, \code{hour}, \code{mday}
  and \code{mon} could have been out of their designated range (say, 0--23
  for hours) and still work correctly, e.g. in conversions and printing.
  This is supported as well, since \R 4.3.0, at least when the values are
  not extreme. %% FIXME: say more

  Function \code{balancePOSIXlt(x)} will now return a version of the
  \code{"POSIXlt"} object \code{x} which by default is balanced in both ways:
  All the internal list components are of full length, and their values are
  inside their ranges as specified in \code{\link{as.POSIXlt}}'s
  \sQuote{Details on POSIXlt}.
  Setting \code{fill.only = TRUE} will only recycle the list components
  to full length, but not check them at all.  This is particularly faster
  when all components of \code{x} are already of full length.

  Experimentally, \code{balancePOSIXlt()} and other functions returning
  \code{POSIXlt} objects now set a \code{\link{logical}} attribute
  \code{"balanced"} with \code{NA} meaning \dQuote{filled-in}, i.e.,
  not \dQuote{ragged} and \code{TRUE} means (fully) balanced.
}
\seealso{
  For more details about many aspects of valid \code{POSIXlt} objects, notably
  their internal list components, see \sQuote{\link{DateTimeClasses}}, e.g.,
  \code{\link{as.POSIXlt}}, notably the section \sQuote{Details on POSIXlt}.
}
\examples{
d1 <- as.POSIXlt("2000-1-2 3:45", tz="UTC")
d1$min <- d1$min + (0:16)*20
(f1 <- format(d1))
str(unclass(d1)) # only $min is long
str(unclass(balancePOSIXlt(d1, fill.only=TRUE) -> df)) # all of length 17; 'min' unchanged
str(        balancePOSIXlt(d1, classed=FALSE)  -> db ) # of length 17 *and* in range
stopifnot(exprs = {
    identical(f1, format(df))
    all.equal(as.POSIXct(d1), as.POSIXct(df))
    identical(f1, format(.POSIXlt(db)))
    identical(as.POSIXct(d1, "UTC"), as.POSIXct(.POSIXlt(db, "UTC")))
})
}
\keyword{utilities}
\keyword{chron}
