\name{file.show}
\alias{file.show}
\title{Display One or More Files}
\usage{
file.show(\dots, header, title="R Information",
          delete.file=FALSE, pager=options()$pager)
}
\arguments{
 \item{\dots}{one or more character vectors containing the names of the
    files to be displayed.}
 \item{header}{vector (of the same length as the number of files
   specified in \code{\dots}) giving a header for each file being
   displayed. Defaults to empty strings.}
 \item{title}{an overall title for the display.  If a separate window
   is used for the display, \code{title} will be used as the
   window title.}
 \item{delete.file}{should the files be deleted after display? Used for
   temporary files.}
 \item{pager}{the pager to be used.}
}
\description{
    This function provides the core of the R help system,
    but it can be used for other purposes as well.
}
\note{
    How the pager is implemented is system dependent. At present
    the Unix version concatentates files and displays them in the pager
    set by the \code{pager} option setting. However, a GUI model might
    be to put up a file in a separate
    window and let the user leave it up while \R continues running. 
}
\author{Ross Ihaka, Brian Ripley.}
\seealso{
\code{\link{file}},
\code{\link{list.files}},
\code{\link{help}}.
}
\examples{
file.show(paste(R.home(),
    "COPYRIGHTS", sep=.Platform$file.sep))
}
\keyword{file}
