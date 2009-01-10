% File src/library/utils/man/windows/choose.files.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
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
  \item{filters}{a matrix of filename filters (not implemented).}
  \item{index}{which row of filters to use by default.}
}
\value{
  A character vector giving zero or more file paths.
}
\details{
  Unlike \code{\link{file.choose}}, \code{tk_choose.files} will always
  attempt to return a character vector giving a list of files.  If the
  user cancels the dialog, then zero files are returned, whereas
  \code{\link{file.choose}} would signal an error.
}
\seealso{
  \code{\link{file.choose}}, \code{\link{tk_choose.dir}}
}

\keyword{file}
