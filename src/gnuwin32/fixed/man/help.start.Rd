\name{help.start}
\title{Hypertext Documentation}
\usage{
help.start(gui = "irrelevant", browser = "irrelevant")
}
\alias{help.start}
\arguments{
 \item{gui}{just for compatibility}
 \item{browser}{the name of the program to be used as hypertext browser.}
}
\description{
  Start the hypertext (currently HTML) version of \R's online documentation.
}
\details{
  The Windows file association mechanism is used to send the HTML file
  to a browser, launching one if necessary.

  Unlike Unix systems, running \code{help.start} does not send all
  future help requests to the browser: use \code{options(htmlhelp=TRUE)}
  to set that.
}
\seealso{
  \code{\link{help}()} for on- and off-line help in ASCII/Editor or
  PostScript format.
}
\examples{
\dontrun{help.start()}
}
\keyword{documentation}
