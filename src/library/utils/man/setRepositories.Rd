\name{setRepositories}
\alias{setRepositories}
\title{Select Package Repositories}
\description{
  Interact with the user to choose the package repositories to be used.
}
\usage{
setRepositories(graphics = TRUE)
}
\arguments{
#ifdef windows
  \item{graphics}{Logical.  Ignored on Windows.}
#endif
#ifdef unix
  \item{graphics}{Logical.  If true and \pkg{tcltk} is available, use a
    Tk widget, otherwise use a text list in the console.}
#endif
}
\details{
  The default list of known repositories is stored in the file
#ifdef unix
  \file{R\_HOME/etc/repositories}.
#endif
#ifdef windows
  \file{R\_HOME/etc/repositories.win}.
#endif
  That file can be edited for a site, or a user can have a personal copy
  in \file{HOME/.R/repositories} which will take precedence.
}
\value{
  None.  This function is invoked for its side effect of updating
  \code{options("repos")}
}

\seealso{
  \code{\link{chooseCRANmirror}}, \code{\link{install.packages}}.
}
\keyword{ utilities }
