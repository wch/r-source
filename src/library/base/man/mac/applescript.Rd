\name{applescript}
\alias{applescript}
\title{Runs Apple's applescripts}
\description{
  This command runs scripts of AppleScript type from within \R
  under a Macintosh.
}
\usage{
applescript(path, scriptname)
}
\arguments{
  \item{path}{name of the folder where to search for the script to
    be executed. If an empty string is passed then \R searches for
    the script in the \code{"script"} folder of \code{R.home()}.}
  \item{scriptname}{the name of the script file.}
}
\seealso{
  \code{\link{Scripts}}.
}
\examples{
applescript("", "demo")
## expand all examples and help files
applescript("", "zip.expand") 
}
\keyword{interface}

