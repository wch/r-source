\name{clipboard}
\alias{readClipboard}
\alias{writeClipboard}
\title{Read/Write Text to/from the Windows Clipboard}
\description{
  Transfer text between a character vector and the Windows clipboard.
}
\usage{
getClipboard()
writeClipboard{str}
}
\arguments{
  \item{str}{a character vector.}
}
\value{
  For \code{readClipboard}, a character vector.
  For \code{writeClipboard} a invisible logical indicating success or failure.
}
\keyword{utility}
