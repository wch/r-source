\name{choose.files}
\alias{choose.files}
\alias{Filters}
\title{Choose a List of Files Interactively}
\description{
  Choose a list of zero or more files interactively.
}
\usage{
choose.files(default = "", caption = "Select files",
	     multi = TRUE, filters = Filters, index = nrow(Filters))

Filters
}
\arguments{
  \item{default}{which filename to show initially}
  \item{caption}{the caption on the file selection dialog}
  \item{multi}{whether to allow multiple files to be selected}
  \item{filters}{a matrix of filename filters (see Details)}
  \item{index}{which row of filters to use by default}
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

  The \code{filters} argument should be an \code{n} by \code{2} character
  matrix.  The first column gives the description the user will see,
  the second column gives the mask(s) to select those files.  If more than
  one mask is used, separate them by semicolons, with no spaces.

  \code{Filters} is a matrix giving the descriptions and masks for
  the file types that \R knows about.
}
\seealso{
  \code{\link{file.choose}}
}
\examples{
if (interactive()) choose.files(filters = Filters[c("zip", "All"),])
}
\keyword{file}
