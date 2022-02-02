% File src/library/utils/man/help.start.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{help.start}
\title{Hypertext Documentation}
\alias{help.start}
\description{
  Start the hypertext (currently HTML) version of \R's online
  documentation.
}
\usage{
help.start(update = FALSE, gui = "irrelevant",
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
    URL as its only argument. This option is normally unset on Windows,
    when the file-association mechanism will be used.}
  \item{remote}{A character string giving a valid URL for the
    \file{\var{\link{R_HOME}}} directory on a remote location.}
}
\details{
  Unless \code{remote} is specified this requires the HTTP server to be
  available (it will be started if possible: see
  \code{\link{startDynamicHelp}}).

  One of the links on the index page is the HTML package index,
  \file{R.home("docs")/html/packages.html}, which can be remade by
  \code{\link{make.packages.html}()}.  For local operation,
  the HTTP server will remake a temporary version of this list when the
  link is first clicked, and each time thereafter check if updating is
  needed (if \code{\link{.libPaths}} has changed or any of the
  directories has been changed).  This can be slow, and using
  \code{update = TRUE} will ensure that the packages list is updated
  before launching the index page.

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
if(.Platform$OS.type == "unix") # includes Mac
  ## the 'remote' arg can be tested by
  help.start(remote = paste0("file://", R.home()))
}}
\keyword{documentation}
