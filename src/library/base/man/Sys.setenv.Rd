% File src/library/base/man/Sys.setenv.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{Sys.setenv}
\alias{Sys.setenv}
\alias{Sys.unsetenv}
\title{Set or Unset Environment Variables}
\usage{
Sys.setenv(\dots)

Sys.unsetenv(x)
}
\arguments{
  \item{\dots}{named arguments with values coercible to a character string.}
  \item{x}{a character vector, or an object coercible to character.}
}
\description{
  \code{Sys.setenv} sets environment variables (for other processes
  called from within \R or future calls to \code{\link{Sys.getenv}} from
  this \R process).

  \code{Sys.unsetenv} removes environment variables.
}
\value{
  A logical vector, with elements being true if (un)setting the
  corresponding variable succeeded.  (For \code{Sys.unsetenv} this
  includes attempting to remove a non-existent variable.)
}
\details{
  The names \code{setenv} and \code{putenv} come from different Unix
  traditions: \R also has \code{Sys.putenv}, but this is now deprecated.
  The internal code uses \code{setenv} if available, otherwise \code{putenv}.
  
  Non-standard \R names must be quoted in \code{Sys.setenv}:
  see the examples.  Most platforms (and POSIX) do not allow names
  containing \code{"="}.
#ifdef Windows
  (Windows does, but the facilities provided by \R may not handle these
  correctly so they should be avoided.)
#endif
  
  There may be system-specific limits on the maximum length of the values
  of individual environment variables or of all environment variables.
#ifdef windows
  Windows has a limit of 32,767 characters on the environment block, and
  \code{cmd.exe} has a limit of 2047 (Windows 2000) or 8192 characters
  (XP and later) for each value.
  % http://support.microsoft.com/kb/830473
#endif
}
\note{
  Not all systems need support \code{Sys.setenv} (although all known
  current platforms do) nor \code{Sys.unsetenv}.
#ifdef windows
  Both are provided on Windows (although C-level \code{unsetenv} is not
  available).
#endif
#ifdef unix
  If \code{Sys.unsetenv} is not supported, it will at least try to
  set the value of the environment variable to \code{""}, with a 
  warning.
#endif
}
\seealso{
  \code{\link{Sys.getenv}}, \link{Startup} for ways to set environment
  variables for the \R session.
  
  \code{\link{setwd}} for the working directory.


  The help for \sQuote{\link{environment variables}} lists many of the
  environment variables used by \R.
}
\examples{
print(Sys.setenv(R_TEST="testit", "A+C"=123))  # `A+C` could also be used
Sys.getenv("R_TEST")
#ifdef windows
Sys.unsetenv("R_TEST")
#endif
#ifdef unix
Sys.unsetenv("R_TEST")  # may warn and not succeed
#endif
Sys.getenv("R_TEST", unset=NA)
}
\keyword{environment}
\keyword{utilities}
