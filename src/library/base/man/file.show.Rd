\name{file.show}
\alias{file.show}
\title{Display One or More Files}
\usage{
file.show(\dots, header, title="R Information",
          delete.file=FALSE, pager=getOption("pager"))
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
    How the pager is implemented is system dependent. 
    The basic Unix version concatentates files and displays them in the pager
    selected by the \code{pager} argument, which is a character vector
    specifying a system command to run on the set of files. GUI models will
    allow special pagers which for instance could
    put up a file in a separate
    window and let the user leave it up while \R continues running. The
    selection of such pagers could either be done using ``magic'' pager
    names being intercepted by lower-level code, or by letting \code{pager} 
    be an R function which will be called with the same arguments as 
    \code{file.show} and take care of interfacing to the GUI.  
}
\author{Ross Ihaka, Brian Ripley.}
\seealso{
\code{\link{files}},
\code{\link{list.files}},
\code{\link{help}}.
}
\examples{
file.show(paste(R.home(),
    "COPYRIGHTS", sep=.Platform$file.sep))
}
\keyword{file}
