\name{warning}
\title{Warning Messages}
\usage{
warning(message)
}
\alias{warning}
\description{
\code{warning} prints the message given as its argument.
}
\examples{
\testonly{eigenval <- 10 ^ -rnorm(1,m=6)}
if(eigenval < 1.e-7) warning("system near singular")
}
\keyword{programming}
