\name{file.edit}
\alias{file.edit}
\title{Opens a file in an editable window}
\description{
  This command is the analogous of the menu item "Open File for Editing"
  of the Macintosh porting of \R but more flexible.  It opens a file 
  in an editable window.
}
\usage{
file.edit(file)
}
\arguments{
  \item{file}{name of the file to open. If an empty string or no arguments is passed 
   then \R opens a clean ready to edit window called "NewFile".
   If the specified file cannot be found or simply does not exist, then a new
   window called "file" will be opened.
   }
}
\seealso{
  \code{\link{new.file}}.
}
\examples{
file.edit("AUTHORS")
## creates a new clean window
file.edit("unexistant")
}
\keyword{interface}

