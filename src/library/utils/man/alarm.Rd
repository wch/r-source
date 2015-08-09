% File src/library/utils/man/alarm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{alarm}
\alias{alarm}
\concept{beep}
\concept{bell}
\title{Alert the User}
\description{
  Gives an audible or visual signal to the user.
}
\usage{
alarm()
}
\details{
  \code{alarm()} works by sending a \code{"\\a"} character to the console.
  On most platforms this will ring a bell, beep, or give some other signal
  to the user (unless standard output has been redirected).

  It attempts to flush the console (see \code{\link{flush.console}}).
}
\value{
  No useful value is returned.
}
\examples{
alarm()
}
\keyword{utilities}

