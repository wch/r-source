\name{setWindowTitle}
\alias{setWindowTitle}
\title{Set the Window Title}
\description{
  Set the title of the R window which will appear in the task bar.
}
\usage{
setWindowTitle(title)
}
\arguments{
  \item{title}{a character string}
}
\details{
  This appends \code{title} to the normal window identification
  (\code{RGui}, \code{R Console} or \code{Rterm}) separated by a colon
  and a space.  Use \code{title = ""} to reset the title.

  This sets the title of the frame in MDI mode, the title of the console
  for \code{RGui --sdi}, and the title of the window from which it was
  launched for \code{Rterm}.  It has no effect in embedded uses of \R.
}
\value{
  \code{NULL} (invisible).
}
\examples{
## show the current working directory in the title.
setWindowTitle(getwd())
## reset the title
setWindowTitle("")
}
\keyword{utilities}
