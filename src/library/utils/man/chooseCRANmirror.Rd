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
#ifdef windows
  \item{graphics}{Logical.  Ignored on Windows.}
#endif
#ifdef unix
  \item{graphics}{Logical.  If true and \pkg{tcltk} is available, use a
    Tk widget, otherwise use \code{\link{menu}}.}
#endif
}
\details{
  The list of mirrors is stored in file \file{R_HOME/doc/CRAN_mirrors.csv}.

  This function was originally written to support a Windows GUI menu
  item, but is also called by \code{\link{contrib.url}} if it finds the
  initial dummy value of \code{\link{options}("repos")}.
}
\value{
  None.  This function is invoked for its side effect of updating
  \code{options("repos")}
}
\seealso{
  \code{\link{contrib.url}}.
}
\keyword{ utilities }
