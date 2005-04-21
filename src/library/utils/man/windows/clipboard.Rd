\name{clipboard}
\alias{readClipboard}
\alias{writeClipboard}
\title{Read/Write Text to/from the Windows Clipboard}
\description{
  Transfer text between a character vector and the Windows clipboard.
}
\usage{
readClipboard()
writeClipboard(str)
}
\arguments{
  \item{str}{a character vector.}
}
\value{
  For \code{readClipboard}, a character vector.
  For \code{writeClipboard} a invisible logical indicating success or failure.
}
\seealso{
  \code{\link{file}} which can be used to set up a connection to a clipboard.
}
\keyword{utilities}
