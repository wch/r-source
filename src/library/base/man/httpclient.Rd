\name{httpclient}
\alias{httpclient}
\title{ Read text from an HTTP server}
\description{
  The function reads text from a URL on an HTTP server into the specified
  file. It checks for HTTP errors but will behave incorrectly if the data
  being read contains binary zeros.  This function is used by the "socket"
  method of \code{\link{download.file}}.
}
\usage{
httpclient(url, port=80, error.is.fatal=TRUE, check.MIME.type=TRUE,
           file=tempfile(), drop.ctrl.z=TRUE)
}
\arguments{
 \item{url}{The URL to read from}
 \item{port}{The port (usually 80)}
 \item{error.is.fatal}{Stop if an error is encountered }
 \item{check.MIME.type}{Require the URL to have a MIME type known to be
   plain ASCII}
 \item{file}{The file to store the text.}
 \item{drop.ctrl.z}{Drop ASCII EOF (CTRL-Z) characters from the text}
}
\value{
  The name of the file containing the downloaded text.
}

\seealso{ \code{\link{download.file}}, \code{\link{read.table.url}} }


\keyword{file}
