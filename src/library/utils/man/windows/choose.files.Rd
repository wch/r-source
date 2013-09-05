% File src/library/utils/man/windows/choose.files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{choose.files}
\alias{choose.files}
\alias{Filters}
\title{Choose a List of Files Interactively}
\description{
  Use a Windows file dialog to choose a list of zero or more files
interactively.
}
\usage{
choose.files(default = "", caption = "Select files",
             multi = TRUE, filters = Filters,
             index = nrow(Filters))

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
  Unlike \code{\link{file.choose}}, \code{choose.files} will always
  attempt to return a character vector giving a list of files.  If the
  user cancels the dialog, then zero files are returned, whereas
  \code{\link{file.choose}} would signal an error.  \code{choose.dir}
  chooses a directory.

  Windows file dialog boxes include a list of \sQuote{filters}, which allow
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
  give a fully qualified file mask (e.g., \code{"c:\\\\*.*"})
  in the \code{default} argument.  If a directory is not given, the
  dialog will start in the current directory the first time, and
  remember the last directory used on subsequent invocations.

  There is a buffer limit on the total length of the selected filenames:
  it is large but this function is not intended to select thousands of
  files, when the limit might be reached.
}
\note{
  This is only available on Windows.
}
\seealso{
  \code{\link{file.choose}}, \code{\link{choose.dir}}.

  \code{\link{Sys.glob}} or \code{\link{list.files}} to select multiple
  files by pattern.
}
\examples{
  if (interactive())
       choose.files(filters = Filters[c("zip", "All"),])
}
\keyword{file}
