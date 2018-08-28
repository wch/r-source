% File src/library/utils/man/unix/nsl.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{nsl}
\alias{nsl}
\title{Look up the IP Address by Hostname (on Unix-alikes)}
\description{
  Interface to the system \command{gethostbyname}, currently available
  only on unix-alikes, i.e., not on Windows.
}
\usage{
nsl(hostname)
}
\arguments{
 \item{hostname}{the name of the host.}
}
\value{
  The IP address, as a character string, or \code{NULL} if the call fails.
}
\details{
  This was included as a test of internet connectivity, to fail if
  the node running \R is not connected.  It will also return \code{NULL}
  if BSD networking is not supported, including the header file
  \file{arpa/inet.h}.

  This function is not available on Windows.
}
\examples{
if(.Platform$OS.type == "unix") # includes Mac
  print( nsl("www.r-project.org") )
}
\keyword{utilities}
