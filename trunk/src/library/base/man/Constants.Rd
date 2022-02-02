% File src/library/base/man/Constants.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{Constants}
\alias{Constants}
\alias{LETTERS}
\alias{letters}
\alias{month.abb}
\alias{month.name}
\alias{pi}
\title{Built-in Constants}
\description{
  Constants built into \R.
}
\usage{
LETTERS
letters
month.abb
month.name
pi
}
\details{
  \R has a small number of built-in constants.

  The following constants are available:
  \itemize{
    \item \code{LETTERS}: the 26 upper-case letters of the Roman
    alphabet;
    \item \code{letters}: the 26 lower-case letters of the Roman
    alphabet;
    \item \code{month.abb}: the three-letter abbreviations for the
    English month names;
    \item \code{month.name}: the English names for the months of the
    year;
    \item \code{pi}: the ratio of the circumference of a circle to its
    diameter.
  }

  These are implemented as variables in the base namespace taking
  appropriate values.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{data}}, \code{\link{DateTimeClasses}}.

  \code{\link{Quotes}} for the parsing of character constants,
  \code{\link{NumericConstants}} for numeric constants.
}
\examples{% earlier R versions said 1705, but most refs say 1706.
## John Machin (ca 1706) computed pi to over 100 decimal places
## using the Taylor series expansion of the second term of
pi - 4*(4*atan(1/5) - atan(1/239))

## months in English
month.name
## months in your current locale
format(ISOdate(2000, 1:12, 1), "\%B")
format(ISOdate(2000, 1:12, 1), "\%b")
}
\keyword{sysdata}
