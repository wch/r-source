\name{read.table.url}
\alias{read.table.url}
\alias{scan.url}
\alias{source.url}
\alias{copy.url}
\alias{url.show}
\title{Read data and code from web server}
\usage{
read.table.url(url, port=80, check.MIME.type=TRUE ...)
scan.url(url, port=80, check.MIME.type=TRUE ...)
source.url(url, port=80, check.MIME.type=TRUE ...)
copy.url(url, port=80, error.is.fatal=TRUE, file=tempfile(),
  check.MIME.type=TRUE, drop.ctrl.z=TRUE)
url.show(url, port=80, check.MIME.type=TRUE, file=tempfile(),
  delete.file=TRUE,...)
}
\arguments{
 \item{url}{URL beginning with \code{http://} }
 \item{port}{ Port number for the HTTP server (usually 80) }
 \item{check.MIME.type}{Check that the document is of a type known to be
     plain ASCII?}
 \item{\dots}{Arguments to pass to \link{read.table},\link{scan},
     \link{source} or \link{file.show}}
 \item{error.is.fatal}{An HTTP error causes an error instead of a
     warning?}
 \item{file}{File to copy to}
 \item{delete.file}{Delete the file?}
 \item{drop.ctrl.z}{Filter out the \code{EOF} (control-Z) character?}
}
\description{
    Extensions of \link{read.table}, \link{scan}, \link{source}
    and \link{file.show} to read text files on a remote HTTP (Web) server.
    HTTP errors are handled correctly, but attempts to read non-text files
    are currently not (thus the \code{check.MIME.type} option).
}
\value{
 \code{copy.url} returns the name of a temporary file.
}

\seealso{
    \code{\link{read.table}}, \code{\link{scan}}, \code{\link{source}},
    \code{\link{make.socket}}, \code{\link{read.socket}},
    \code{\link{file.show}}
}

\examples{
\dontrun{read.table.url("http://lib.stat.cmu.edu/jcgs/tu",skip=4,header=T)}
\dontrun{url.show("http://lib.stat.cmu.edu/datasets/csb/ch11b.txt")}
\dontrun{beaver<-read.table.url("http://lib.stat.cmu.edu/datasets/csb/ch11b.dat",
  col.names=c("obsnum","day","time","temperature","activity"),row.names=1)}
\dontrun{url.show("http://lib.stat.cmu.edu/datasets/csb/ch3a.txt")}
\dontrun{ozone<-read.table.url("http://lib.stat.cmu.edu/datasets/csb/ch3a.dat",
  col.names=c("date","day.cts","day.passive","night.cts","night.passive"),
  na.strings=".")}
}
\keyword{file}
\keyword{misc}
