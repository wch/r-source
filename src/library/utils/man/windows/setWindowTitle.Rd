% File src/library/utils/man/windows/setWindowTitle.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{setWindowTitle}
\alias{setWindowTitle}
\alias{getWindowTitle}
\alias{getIdentification}
\alias{setStatusBar}
\title{Set or get the Window Title, or Set the Statusbar}
\description{
  Set the title of the R window which will appear in the task bar, or of
  the statusbar (if in use).
}
\usage{
setWindowTitle(suffix, title = paste(getIdentification(), suffix))

getWindowTitle()

getIdentification()

setStatusBar(text)
}
\arguments{
  \item{suffix}{a character string to form part of the title}
  \item{title}{a character string forming the complete new title}
  \item{text}{a character string of up to 255 characters, to be
    displayed in the status bar.}
}
\details{
  \code{setWindowTitle} appends \code{suffix} to the normal window
  identification (\code{RGui}, \code{R Console} or \code{Rterm}).  Use
  \code{suffix = ""} to reset the title.

  \code{getWindowTitle} gets the current title.

  This sets the title of the frame in MDI mode, the title of the console
  for \code{RGui --sdi}, and the title of the window from which it was
  launched for \code{Rterm}.
  It has no effect in embedded uses of \R.

  \code{getIdentification} returns the normal window identification.

  \code{setStatusBar} sets the text in the statusbar of an MDI
  frame: if this is not currently shown it is selected and shown.
}
\value{
  The first three functions return a length 1 character vector.

  \code{setWindowTitle} returns the previous window title (invisibly).

  \code{getWindowTitle} and \code{getIdentification} return the current
  window title and the normal window identification, respectively.
}
\note{
  This is only available on Windows.
}
\examples{
## show the current working directory in the title, saving the old one
oldtitle <- setWindowTitle(getwd())
Sys.sleep(0.5)
## reset the title
setWindowTitle("")
Sys.sleep(0.5)
## restore the original title
setWindowTitle(title = oldtitle)
}
\keyword{utilities}
