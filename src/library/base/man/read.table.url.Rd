\name{read.table.url}
\alias{read.table.url}
\alias{scan.url}
\alias{source.url}
\alias{url.show}
\title{Read Data and Code from a URL}
\usage{
read.table.url(url, method = "auto",\dots)
scan.url(url, file = tempfile(), method = "auto", \dots)
source.url(url, file = tempfile(), method = "auto", \dots)
url.show(url, title = url, file = tempfile(),
         delete.file = TRUE, method = "auto", \dots)
}
\arguments{
  \item{url}{The URL to read from}
  \item{method}{File transfer method: see \code{\link{download.file}}}
  \item{\dots}{Arguments to pass to \code{\link{read.table}},
    \code{\link{scan}}, \code{\link{source}} or
    \code{\link{file.show}}.}
  \item{file}{File to copy to.}
  \item{delete.file}{Delete the file afterwards?}
}
\description{
  Extensions of \code{\link{read.table}}, \code{\link{scan}},
  \code{\link{source}}
  and \code{\link{file.show}} to read text files on a remote 
  server.
}
\details{
  These functions call \code{\link{download.file}} to create a temporary
  local file. The file can be downloaded by \code{lynx} or \code{wget}
  if these are available on the system. Another option is a direct
  \code{HTTP} socket connection, if the local machine allows this. 
}
\value{
  The same value as the respective file-based functions.
}

\seealso{
  \code{\link{read.table}}, \code{\link{scan}}, \code{\link{source}},
  \code{\link{make.socket}}, \code{\link{read.socket}},
  \code{\link{file.show}},\code{\link{download.file}}
}

\examples{
\dontrun{read.table.url("http://lib.stat.cmu.edu/jcgs/tu",
   skip=4, header=TRUE)}
\dontrun{url.show("http://lib.stat.cmu.edu/datasets/csb/ch11b.txt")}
\dontrun{beaver<-read.table.url("http://lib.stat.cmu.edu/datasets/csb/ch11b.dat",
  col.names=c("obsnum","day","time","temperature","activity"), row.names=1)}
# the next two examples will only work if socket connections to
# statlib are allowed from your site.
\dontrun{url.show("http://lib.stat.cmu.edu/datasets/csb/ch3a.txt",
                  method="socket")}
\dontrun{ozone<-read.table.url("http://lib.stat.cmu.edu/datasets/csb/ch3a.dat",
  col.names=c("date","day.cts","day.passive","night.cts","night.passive"),
  na.strings=".", method="socket")}
}
\keyword{file}
\keyword{misc}
