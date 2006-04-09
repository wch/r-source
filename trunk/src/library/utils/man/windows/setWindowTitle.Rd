\name{setWindowTitle}
\alias{setWindowTitle}
\alias{getWindowTitle}
\alias{getIdentification}
\title{Set or get the Window Title}
\description{
  Set the title of the R window which will appear in the task bar.
}
\usage{
setWindowTitle(suffix, title = paste(getIdentification(), suffix))

getWindowTitle()

getIdentification()
}
\arguments{
  \item{suffix}{a character string to form part of the title}
  \item{title}{a character string forming the complete new title}
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
}
\value{
  All three functions return a length 1 character vector.
  
  \code{setWindowTitle} returns the previous window title (invisibly).  
  
  \code{getWindowTitle} and \code{getIdentification} return the current
  window title and the normal window identification, respectively.
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
