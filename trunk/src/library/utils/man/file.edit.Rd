\name{file.edit}
\alias{file.edit}
\title{Edit One or More Files}
\description{Edit one or more files in a text editor.}
\usage{
file.edit(\dots, title = file, editor = getOption("editor"))
}
\arguments{
  \item{\dots}{one or more character vectors containing the names of the
    files to be edited.}
  \item{title}{the title to use in the editor; defaults to the filename.}  
  \item{editor}{the text editor to be used.}
}
\details{
    The behaviour of this function is very system dependent.  Currently files can be opened 
    only one at a time on Unix; on Windows, the internal editor allows multiple files to be
    opened, but has a limit of 50 simultaneous edit windows.
    
    The \code{title} argument is used for the window caption in Windows, and is ignored
    on other platforms.
}
\seealso{
  \code{\link{files}},
  \code{\link{file.show}},
  \code{\link{edit}},
  \code{\link{fix}},
}
\examples{
\dontrun{
# open two R scripts for editing
file.edit("script1.R", "script2.R")
}
}
\keyword{utilities}
