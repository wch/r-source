\name{machine}
\title{Determine the Machine R is Running On}
\usage{
machine()
}
\alias{machine}
\description{
  This function returns a character string which specifies what kind of
  environment \R is being run in.
}
\value{
  A character string, one of \code{"Unix"}, \code{"Macintosh"} or
  \code{"Win32"}.  The `darwin' MacOS X port returns \code{"Unix"}.
}
\seealso{
  \code{\link{.Platform}} which provides more than \code{machine()};
  \code{\link{Machine}} for the characteristics of the platform's arithmetic.
}
\examples{
machine()# to see yours
tolower(machine()) == .Platform $OS.type ## --> TRUE except on Windows.
if (machine() == "Macintosh")
  cat("You are using a Macintosh computer\n")
}
\keyword{environment}
\keyword{programming}
