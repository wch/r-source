% File src/library/base/man/Sys.info.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Copyright      2008 The R Foundation
% Distributed under GPL 2 or later

\name{Sys.info}
\alias{Sys.info}
\title{Extract System and User Information}
\description{
  Reports system and user information.
}
\usage{
Sys.info()
}
\value{
  A character vector with fields
  \item{sysname}{The operating system.}
  \item{release}{The OS release.}
  \item{version}{The OS version.}
  \item{nodename}{A name by which the machine is known on the network (if
    any).}
  \item{machine}{A concise description of the hardware.}
  \item{login}{The user's login name, or \code{"unknown"} if it cannot be
    ascertained.}
  \item{user}{The name of the real user ID, or \code{"unknown"} if it
    cannot be ascertained.}
#ifdef unix
  The first five fields come from the \code{uname(2)} system call.  The
  login name comes from \code{getlogin(2)}, and the user name from
  \code{getpwuid(getuid())}
#endif
}
\details{
  This function is not implemented on all \R platforms, and returns
  \code{NULL} when not available.  Where possible it is based on POSIX
  system calls.
#ifdef windows
  (Under Windows, it is obtained from Windows system calls.)
#endif

  \code{Sys.info()} returns details of the platform \R is running on,
  whereas \code{\link{R.version}} gives details of the platform \R was
  built on: they may well be different.
}
\note{
  The meaning of OS \sQuote{release} and \sQuote{version} is
  system-dependent and there is no guarantee that the node or
  login or user names will be what you might reasonably expect.
  (In particular on some Linux distributions the login name is unknown
  from sessions with re-directed inputs.)
}

\seealso{
  \code{\link{.Platform}}, and \code{\link{R.version}}.
  \code{\link{sessionInfo}()} gives a synopsis of both your system and
  the \R session.
}

\examples{
Sys.info()
## An alternative (and probably better) way to get the login name on Unix
Sys.getenv("LOGNAME")
}
\keyword{utilities}
