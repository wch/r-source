% File src/library/utils/man/windows/choose.dir.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2007-10 R Core Team
% Distributed under GPL 2 or later

\name{choose.dir}
\alias{choose.dir}
\title{Choose a Folder Interactively}
\description{
  Use a Windows shell folder widget to choose a folder interactively.
}
\usage{
choose.dir(default = "", caption = "Select folder")
}
\arguments{
  \item{default}{which folder to show initially.}
  \item{caption}{the caption on the selection dialog.}
}
\value{
  A length-one character vector, character \code{NA} if
  \sQuote{Cancel} was selected.
}
\details{
  This brings up   the Windows shell folder selection widget.  With the
  default \code{default = ""}, \sQuote{My Computer} (or similar) is
  initially selected.

  To workaround a bug, on Vista and later only folders under
  \sQuote{Computer} are accessible via the widget.
}
\note{
  This is only available on Windows.
}
\seealso{
  \code{\link{choose.files}}
}
\examples{
  if (interactive())
        choose.dir(getwd(), "Choose a suitable folder")
}
\keyword{file}
