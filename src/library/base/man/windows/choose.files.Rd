\name{choose.files}
\alias{choose.files}
\title{Choose a List of Files Interactively}
\description{
 Choose a list of zero or more files interactively.
}
\usage{
choose.files(mask = '*.*', caption = 'Select files')
}
\arguments{
  \item{mask}{which files to show initially}
  \item{caption}{the caption on the file selection dialog}
}
\value{
  A character vector giving zero or more file paths.
}
\details{
Unlike \code{\link{file.choose}}, this function will always 
attempt to return a character vector
giving a list of files.  If the user cancels the dialog, then
zero files are returned, whereas \code{\link{file.choose}} would 
signal an error.
}
\seealso{\code{\link{file.choose}}}
\keyword{file}
