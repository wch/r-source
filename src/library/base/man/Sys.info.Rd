\name{Sys.info}
\alias{Sys.info}
\title{Extract System and User Information}
\description{
  Reports system and user information.
}
\usage{
Sys.info()
}
\details{
   This function is not implemented on all \R platforms, and returns
  \code{NULL} when not available.  Where possible it is based on POSIX
  system calls.
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
#ifdef windows
The information is obtained from Windows system calls.  It is likely to
be most complete on Windows NT/2000/XP systems.
#endif
}
\note{
  The meaning of OS ``release'' and ``version'' is highly
  system-dependent and there is no guarantee that the node or
  login or user names will be what you might reasonably expect.
  (In particular on some Linux distributions the login name is unknown
  from sessions with re-directed inputs.)
}
\author{B. D. Ripley}

\seealso{\code{\link{.Platform}}}

\examples{
Sys.info()
## An alternative (and probably better) way to get the login name on Unix
Sys.getenv("LOGNAME")
}
\keyword{utilities}
