% File src/library/base/man/file.show.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{file.show}
\alias{file.show}
\title{Display One or More Files}
\description{Display one or more files.}
\usage{
file.show(\dots, header = rep("", nfiles),
          title = "R Information",
          delete.file = FALSE, pager = getOption("pager"),
          encoding = "")
}
\arguments{
  \item{\dots}{one or more character vectors containing the names of the
    files to be displayed.  Paths with have \link{tilde expansion}.}
  \item{header}{character vector (of the same length as the number of files
    specified in \code{\dots}) giving a header for each file being
    displayed.  Defaults to empty strings.}
  \item{title}{an overall title for the display.  If a single separate
    window is used for the display, \code{title} will be used as the window
    title.  If multiple windows are used, their titles should combine the title
    and the file-specific header.}
  \item{delete.file}{should the files be deleted after display?  Used
    for temporary files.}
  \item{pager}{the pager to be used: not used on all platforms}
  \item{encoding}{character string giving the encoding to be assumed for
    the file(s).}
}
\details{
  This function provides the core of the R help system, but it can be
  used for other purposes as well, such as \code{\link{page}}.

  How the pager is implemented is highly system-dependent.

  The basic Unix version concatenates the files (using the headers) to a
  temporary file, and displays it in the pager selected by the
  \code{pager} argument, which is a character vector specifying a system
  command (a full path or a command found on the \env{PATH}) to run on
  the set of files.  The \sQuote{factory-fresh} default is to use
  \file{R_HOME/bin/pager}, which is a shell script running the command-line
  specified by the environment variable \env{PAGER} whose default is set
  at configuration, usually to \command{less}.  On a Unix-alike
  \command{more} is used if \code{pager} is empty.

  Most GUI systems will use a separate pager window for each file, and
  let the user leave it up while \R continues running.  The selection of
  such pagers could either be done using special pager names being
  intercepted by lower-level code (such as \code{"internal"} and
  \code{"console"} on Windows), or by letting \code{pager} be an \R
  function which will be called with arguments \code{(files, header,
    title, delete.file)} corresponding to the first four arguments of
  \code{file.show} and take care of interfacing to the GUI.

  The \command{R.app} GUI on OS X uses its internal pager irrespective
  of the setting of \code{pager}.

  Not all implementations will honour \code{delete.file}.  In
  particular, using an external pager on Windows does not, as there is
  no way to know when the external application has finished with the
  file.
}
\author{Ross Ihaka, Brian Ripley.}
\seealso{
  \code{\link{files}},
  \code{\link{list.files}},
  \code{\link{help}}.

  \code{\link{file.edit}}.
}
\examples{
file.show(file.path(R.home("doc"), "COPYRIGHTS"))
}
\keyword{file}
