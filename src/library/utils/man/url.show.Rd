\name{url.show}
\alias{url.show}
\title{Display a text URL}
\description{
  Extension of \code{\link{file.show}} to display text files from a remote 
  server.
}
\usage{
url.show(url, title = url, file = tempfile(),
         delete.file = TRUE, method, \dots)
}
\arguments{
  \item{url}{The URL to read from.}
  \item{title}{Title for the browser.}
  \item{file}{File to copy to.}
  \item{delete.file}{Delete the file afterwards?}
  \item{method}{File transfer method: see \code{\link{download.file}}}
  \item{\dots}{Arguments to pass to \code{\link{file.show}}.}
}
\seealso{
  \code{\link{url}}, \code{\link{file.show}}, \code{\link{download.file}}
}
\examples{
\dontrun{url.show("http://lib.stat.cmu.edu/datasets/csb/ch3a.txt")}
}
\keyword{file}
\keyword{misc}
