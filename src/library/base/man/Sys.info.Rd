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
  \item{login}{The user name.}
#ifdef unix
The first five fields come from the \code{uname(2)} system call. The
login name comes from \code{getlogin}.
#endif
#ifdef unix
The information is obtained from Windows system calls. It is likely to
be most complete on Windows NT systems.
#endif
}
\author{B. D. Ripley}

\seealso{\code{\link{.Platform}}}

\examples{
Sys.info()
}
\keyword{utilities}
