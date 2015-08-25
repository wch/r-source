% File src/library/utils/man/windows/choose.dir.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2007-9 R Core Team
% Distributed under GPL 2 or later

\name{tk_choose.dir}
\alias{tk_choose.dir}
\title{Choose a Folder Interactively}
\description{
  Use a Tk widget to choose a directory interactively.
}
\usage{
tk_choose.dir(default = "", caption = "Select directory")
}
\arguments{
  \item{default}{which directory to show initially.}
  \item{caption}{the caption on the selection dialog.}
}
\value{
  A length-one character vector, character \code{NA} if
  \sQuote{Cancel} was selected.
}
\seealso{
  \code{\link{tk_choose.files}}
}
\examples{\dontrun{
tk_choose.dir(getwd(), "Choose a suitable folder")
}}
\keyword{file}
