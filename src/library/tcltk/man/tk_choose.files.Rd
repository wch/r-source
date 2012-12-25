% File src/library/utils/man/windows/choose.files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{tk_choose.files}
\alias{tk_choose.files}
\title{Choose a List of Files Interactively}
\description{
  Use a Tk file dialog to choose a list of zero or more files
  interactively.
}
\usage{
tk_choose.files(default = "", caption = "Select files",
                multi = TRUE, filters = NULL, index = 1)
}
\arguments{
  \item{default}{which filename to show initially.}
  \item{caption}{the caption on the file selection dialog.}
  \item{multi}{whether to allow multiple files to be selected.}
  \item{filters}{two-column character matrix of filename filters.}
  \item{index}{unused.}
}
\value{
  A character vector giving zero or more file paths.
}
\details{
  Unlike \code{\link{file.choose}}, \code{tk_choose.files} will always
  attempt to return a character vector giving a list of files.  If the
  user cancels the dialog, then zero files are returned, whereas
  \code{\link{file.choose}} would signal an error.

  The format of \code{filters} can be seen from the example.  File
  patterns are specified via extensions, with \code{"*"} meaning any
  file, and \code{""} any file without an extension (a filename not
  containing a period).  (Other forms may work on specific platforms.)
  Note that the way to have multiple extensions for one file type is to
  have multiple rows with the same name in the first column, and that
  whether the extensions are named in file chooser widget is
  platform-specific. \bold{The format may change before release.}
}
\note{
  A bug in Tk 8.5.0--8.5.4 prevented multiple selections being used.
}

\seealso{
  \code{\link{file.choose}}, \code{\link{tk_choose.dir}}
}
\examples{
Filters <- matrix(c("R code", ".R", "R code", ".s",
                    "Text", ".txt", "All files", "*"),
                  4, 2, byrow = TRUE)
Filters
if(interactive()) tk_choose.files(filter = Filters)
}
\keyword{file}
