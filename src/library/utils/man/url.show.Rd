% File src/library/utils/man/url.show.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{url.show}
\alias{url.show}
\title{Display a Text URL}
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
\note{
  Since this is for text files, it will convert to CRLF line endings on
  Windows.
}
\seealso{
  \code{\link{url}}, \code{\link{file.show}}, \code{\link{download.file}}
}
\examples{
\dontrun{url.show("http://www.stats.ox.ac.uk/pub/datasets/csb/ch3a.txt")}
}
\keyword{file}
\keyword{misc}
