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
  \item{graphics}{Logical.
#ifdef windows
    If true use a listbox,
#endif
#ifdef unix
    If true and \pkg{tcltk} and an X server are available, use a Tk widget,
#endif
    otherwise use a text list in the console.}
}
\details{
  The default list of known repositories is stored in the file
  \file{R\_HOME/etc/repositories}.
  That file can be edited for a site, or a user can have a personal copy
  in \file{HOME/.R/repositories} which will take precedence.

  The items that are preselected are those that are currently in
  \code{options("repos")} plus those marked as default in the
  list of known repositories.
}
\value{
  None.  This function is invoked for its side effect of updating
  \code{options("repos")}
}

\seealso{
  \code{\link{chooseCRANmirror}}, \code{\link{install.packages}}.
}
\keyword{ utilities }
