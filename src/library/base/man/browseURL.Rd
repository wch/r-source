\name{browseURL}
\alias{browseURL}
\title{Load URL into a WWW Browser}
\description{
  Load a given URL into a WWW browser.
}
\usage{
browseURL(url, browser = getOption("browser"))
}
\arguments{
  \item{url}{a non-empty character string giving the URL to be loaded.}
  \item{browser}{a non-empty character string giving the name of the
    program to be used as hypertext browser.  It should be in the PATH,
    or a full path specified.
#ifdef windows
    
    Under Windows \code{NULL} is also allowed (and is the default), and
    implies that the file association mechanism will be used.
#endif
#ifdef mac
    
    Under Macintosh the browser should be the complete name of
    the application but without path, i.e. "Internet Explorer".
    The \code{NULL} is allowed (and is the default) which implies
    that the standard system browser will be used.
#endif

    }
}
#ifdef windows
\examples{
\dontrun{browseURL("http://www.r-project.org")
browseURL("file://c|/R/rw1060/doc/html/index.html",
          browser="C:/Program Files/Netscape/Netscape/netscp.exe")
}}
#endif
\keyword{file}
