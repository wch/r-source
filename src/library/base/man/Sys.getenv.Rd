% File src/library/base/man/Sys.getenv.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Sys.getenv}
\title{Get Environment Variables}
\usage{
Sys.getenv(x = NULL, unset = "", names = NA)
}
\alias{Sys.getenv}
\arguments{
  \item{x}{a character vector, or \code{NULL}.}
  \item{unset}{a character string.}
  \item{names}{logical: should the result be named?  If \code{NA} (the
    default) single-element results are not named whereas multi-element
    results are.}
}
\description{
  \code{Sys.getenv} obtains the values of the environment variables.
}
\details{
  Both arguments will be coerced to character if necessary.

  Setting \code{unset = NA} will enable unset variables and those set to
  the value \code{""} to be distinguished, \emph{if the OS does}.  POSIX
  requires the OS to distinguish, and all known current \R platforms do.
}
\value{
  A vector of the same length as \code{x}, with (if \code{names ==
  TRUE}) the variable names as its \code{names} attribute.  Each element
  holds the value of the environment variable named by the corresponding
  component of \code{x} (or the value of \code{unset} if no environment
  variable with that name was found).

  On most platforms \code{Sys.getenv()} will return a named vector
  giving the values of all the environment variables, sorted in the
  current locale.  It may be confused by names containing \code{=} which
  some platforms allow but POSIX does not.  (Windows is such a platform:
  there names including \code{=} are truncated just before the first
  \code{=}.)

  When \code{x} is missing and \code{names} is not false, the result is
  of class \code{"DList"} in order to get a nice
  \code{\link{print}} method.
}
\seealso{
  \code{\link{Sys.setenv}},
  \code{\link{Sys.getlocale}} for the locale in use,
  \code{\link{getwd}} for the working directory.

  The help for \sQuote{\link{environment variables}} lists many of the
  environment variables used by \R.
}
\examples{
## whether HOST is set will be shell-dependent e.g. Solaris' csh does not.
Sys.getenv(c("R_HOME", "R_PAPERSIZE", "R_PRINTCMD", "HOST"))

names(s <- Sys.getenv()) # all settings (the values could be very long)
head(s, 12)# using the DList print() method

## Language and Locale settings -- but rather use Sys.getlocale()
s[grep("^L(C|ANG)", names(s))]
}
\keyword{environment}
\keyword{utilities}
