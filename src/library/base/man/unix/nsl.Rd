\name{nsl}
\alias{nsl}
\title{Look up the IP Address by Hostname}
\description{
  Interface to \code{gethostbyname}.
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
\note{
  This was included as a test of internet connectivity, to fail if
  the node running R is not connected.
}
\examples{
\dontrun{nsl("www.r-project.org")}
}
\keyword{utilities}
