\name{toHTML}
\alias{toHTML}
\alias{toHTML.packageIQR}
\title{
Display an object in HTML.
}
\description{
This generic function generates a complete HTML page from an object.
}
\usage{
toHTML(x, ...)
\S3method{toHTML}{packageIQR}(x, ...)
}
\arguments{
  \item{x}{ An object to display. }
  \item{\dots}{ Optional parameters for methods. }
}
\value{
A character vector to display the object \code{x}.  The \code{"packageIQR"} method
is designed to display lists in the \R{} help system.
}
\seealso{
\code{\link{HTMLheader}}
}
\examples{
cat(toHTML(demo(package="base")), sep="\n")
}
\keyword{ utilities }
\keyword{ documentation }
