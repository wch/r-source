\name{file.show}
\alias{file.show}
\title{Display One or More Files}
\usage{
file.show(file, header=rep("", length(file)),
    title="R Information")
}
\arguments{
 \item{file}{a vector containing the names of the files to be displayed.}
 \item{header}{vector (of the same length as \code{file}) giving a header
   for each file being displayed.}
 \item{title}{an overall title for the display.  If a separate window
   is used for the display, \code{title} will be used as the
   window title.}
}
\description{
This function provides the core of the R help system,
but it can be used for other purposes as well.
}
\author{Ross Ihaka.}
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
