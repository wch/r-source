% File src/library/base/man/Sys.info.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
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
  \item{sysname}{The operating system name.}
  \item{release}{The OS release.}
  \item{version}{The OS version.}
  \item{nodename}{A name by which the machine is known on the network (if
    any).}
  \item{machine}{A concise description of the hardware, often the CPU type.}
  \item{login}{The user's login name, or \code{"unknown"} if it cannot be
    ascertained.}
  \item{user}{The name of the real user ID, or \code{"unknown"} if it
    cannot be ascertained.}
  \item{effective_user}{The name of the effective user ID, or
    \code{"unknown"} if it
    cannot be ascertained.  This may differ from the real user in
    \sQuote{set-user-ID} processes.}
#ifdef unix

  The first five fields come from the \code{uname(2)} system call.  The
  login name comes from \code{getlogin(2)}, and the user names from
  \code{getpwuid(getuid())} and \code{getpwuid(geteuid())}
#endif
}
\details{
  This function is not implemented on all \R platforms, and returns
  \code{NULL} when not available.  Where possible it is based on POSIX
  system calls.  (Under Windows, it is obtained by Windows system
  calls and the last three values are the same.)

  \code{Sys.info()} returns details of the platform \R is running on,
  whereas \code{\link{R.version}} gives details of the platform \R was
  built on: the \code{release} and \code{version} may well be different.

  Note that OS names might not be what you expect: for example OS X
  identifies itself as \samp{Darwin} and Solaris as \samp{SunOS}.
}
\note{
  The meaning of OS \sQuote{release} and \sQuote{version} is
  system-dependent and there is no guarantee that the node or
  login or user names will be what you might reasonably expect.
  (In particular on some Linux distributions the login name is unknown
  from sessions with re-directed inputs.)

  The use of alternatives such as \code{system("whoami")} is not
  portable: the POSIX command \code{system("id")} is much more portable
  on Unix-alikes, provided only the POSIX options are used (and not the
  many BSD and GNU extensions).
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
