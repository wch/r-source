\name{choose.files}
\alias{choose.files}
\alias{choose.dir}
\alias{Filters}
\title{Choose a List of Files Interactively}
\description{
  Use a Windows file dialog to choose a list of zero or more files 
interactively.
}
\usage{
choose.files(default = "", caption = "Select files",
	     multi = TRUE, filters = Filters, index = nrow(Filters))

Filters

choose.dir(default = "", caption = "Select folder")
}
\arguments{
  \item{default}{which filename to show initially}
  \item{caption}{the caption on the file selection dialog}
  \item{multi}{whether to allow multiple files to be selected}
  \item{filters}{a matrix of filename filters (see Details)}
  \item{index}{which row of filters to use by default}
}
\value{
  For \code{choose.files}, a character vector giving zero or more file
  paths.

  For \code{choose.dir}, a character string, \code{NA} if
  \sQuote{Cancel} was selected. 
}
\details{
  Unlike \code{\link{file.choose}}, \code{choose.files} will always
  attempt to return a character vector giving a list of files.  If the
  user cancels the dialog, then zero files are returned, whereas
  \code{\link{file.choose}} would signal an error.  \code{choose.dir}
  chooses a directory.

  Windows file dialog boxes include a list of \dQuote{filters}, which allow
  the file selection to be limited to files of specific types.
  The \code{filters} argument to \code{choose.files} allows the list
  of filters to be set.  It should be an \code{n} by \code{2} character
  matrix.  The first column gives, for each filter, the description the
  user will see, while the second column gives the mask(s) to select
  those files.  If more than one mask is used, separate them by
  semicolons, with no spaces.  The \code{index} argument chooses which
  filter will be used initially.

  \code{Filters} is a matrix giving the descriptions and masks for
  the file types that \R knows about.  Print it to see typical formats
  for filter specifications.  The examples below show how particular
  filters may be selected.

  If you would like to display files in a particular directory,
  give a fully qualified file mask (e.g., \code{'c:\\\\*.*'})
  in the \code{default} argument.  If a directory is not given, the
  dialog will start in the current directory the first time, and
  \dQuote{remember} the last directory on subsequent invocations.
}
\seealso{
  \code{\link{file.choose}}
}
\examples{
  if (interactive()) 
	choose.files(filters = Filters[c("zip", "All"),])

  if (interactive()) 
	choose.dir(getwd(), "Choose a suitable folder")
}
\keyword{file}
