\name{alarm}
\alias{alarm}
\concept{beep}
\concept{bell}
\title{Alert the user}
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
}
\value{
No useful value is returned.
}
\examples{
alarm()
}
\keyword{utilities}

