\name{file.show}
\alias{file.show}
\title{Display One or More Files}
\description{Display one or more files.}
\usage{
file.show(\dots, header = rep("",nfiles), title = "R Information",
          delete.file=FALSE, pager=getOption("pager"))
}
\arguments{
  \item{\dots}{one or more character vectors containing the names of the
    files to be displayed.}
  \item{header}{character vector (of the same length as the number of files
    specified in \code{\dots}) giving a header for each file being
    displayed.  Defaults to empty strings.}
  \item{title}{an overall title for the display.  If a single separate
    window is used for the display, \code{title} will be used as the window
    title. If multiple windows are used, their titles should combine the title
    and the file-specific header.}
  \item{delete.file}{should the files be deleted after display?  Used
    for temporary files.}
  \item{pager}{the pager to be used.}
}
\details{
  This function provides the core of the R help system, but it can be
  used for other purposes as well.
}
\note{
  How the pager is implemented is highly system dependent.

  The basic Unix version concatenates the files (using the headers) to a
  temporary file, and displays it in the
  pager selected by the \code{pager} argument, which is a character
  vector specifying a system command to run on the set of files.

  Most GUI systems will use a separate pager window for each file,
  and let the user leave it up while \R
  continues running.  The selection of such pagers could either be done
  using \dQuote{magic} pager names being intercepted by lower-level code
  (such as \code{"internal"} and \code{"console"} on Windows), or
  by letting \code{pager} be an R function which will be called with the
  same arguments as \code{file.show} and take care of interfacing to the
  GUI.

  Not all implementations will honour \code{delete.file}.
#ifdef windows
  In particular, using an external pager on Windows does not, as there
  is no way to know when the external application has finished with
  the file.
#endif
}
\author{Ross Ihaka, Brian Ripley.}
\seealso{
  \code{\link{files}},
  \code{\link{list.files}},
  \code{\link{help}}.
}
\examples{
file.show(file.path(R.home(), "COPYRIGHTS"))
}
\keyword{file}
