\name{chooseCRANmirror}
\alias{chooseCRANmirror}
\title{Select a CRAN Mirror}
\description{
  Interact with the user to choose a CRAN mirror.
}
\usage{
chooseCRANmirror(graphics = TRUE)
}
\arguments{
  \item{graphics}{Logical.
#ifdef windows
    If true use a Windows list box,
#endif
#ifdef unix
    If true and \pkg{tcltk} and an X server are available, use a Tk
    widget, or if under the AQUA interface use a MacOS X widget,
#endif
    otherwise use \code{\link{menu}}.}
}
\details{
  A list of mirrors is stored in file
  \file{R\_HOME/doc/CRAN\_mirrors.csv}, but first an on-line list of
  current mirrors is consulted, and the file copy used only if the
  on-line list is inaccessible.

  This function was originally written to support a Windows GUI menu
  item, but is also called by \code{\link{contrib.url}} if it finds the
  initial dummy value of \code{\link{options}("repos")}.
}
\value{
  None.  This function is invoked for its side effect of updating
  \code{options("repos")}
}
\seealso{
  \code{\link{setRepositories}}, \code{\link{contrib.url}}.
}
\keyword{ utilities }
