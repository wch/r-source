% File src/library/utils/man/help.start.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{help.start}
\title{Hypertext Documentation}
\alias{help.start}
\description{
  Start the hypertext (currently HTML) version of \R's online
  documentation.
}
\usage{
help.start(update = TRUE, gui = "irrelevant",
           browser = getOption("browser"), remote = NULL)
}
\arguments{
  \item{update}{logical: should this attempt to update the package index to
    reflect the currently available packages.  (Not attempted if
    \code{remote} is non-\code{NULL}.)}
  \item{gui}{just for compatibility with S-PLUS.}
  \item{browser}{the name of the program to be used as hypertext
    browser.  It should be in the \env{PATH}, or a full path specified.
    Alternatively, it can be an \R function which will be called with a
    URL as its only argument.
#ifdef windows
    This option is normally unset on Windows, when the file-association
    mechanism will be used.
#endif
  }
  \item{remote}{A character string giving a valid URL for the
    \file{\var{\link[base:Rhome]{R_HOME}}} directory on a remote location.}
}
\details{
  Unless \code{remote} is specified this requires the HTTP server to be
  available (it will be started if possible: see
  \code{\link{startDynamicHelp}}).
  
  Unless argument \code{remote} is used or \code{update = FALSE}, the
  package list is re-made each time \code{\link{make.packages.html}} or
  \code{help.start} is run.  If the HTTP server is in use, that will
  call \code{\link{make.packages.html}} whenever the packages index page
  is accessed, and that will check if updating is needed.  So using
  \code{update = FALSE} is fine provided you are prepared to wait for
  the package list to be created when first used.

  Argument \code{remote} can be used to point to HTML help published by
  another \R installation: it will typically only show packages from the
  main library of that installation.
}

\seealso{
  \code{\link{help}()} for on- and off-line help in other formats.

  \code{\link{browseURL}} for how the help file is displayed.

  \code{\link{RSiteSearch}} to access an on-line search of \R resources.
}
\examples{
\donttest{help.start()}
\dontrun{
#ifdef unix
## the 'remote' arg can be tested by
help.start(remote=paste("file://", R.home(), sep=""))
#endif
}}
\keyword{documentation}
