\name{file.edit}
\alias{file.edit}
\title{Edit One or More Files}
\description{Edit one or more files in a text editor.}
\usage{
file.edit(\dots, editor=getOption("editor"))
}
\arguments{
  \item{\dots}{one or more character vectors containing the names of the
    files to be edited.}
  \item{editor}{the text editor to be used.}
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
