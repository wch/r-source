\name{help.start}
\title{Hypertext Documentation}
\usage{
#ifdef unix  
help.start(gui = "irrelevant", browser = getOption("browser"), remote = NULL)
#endif
#ifdef windows  
help.start(gui = "irrelevant", browser = "irrelevant")
#endif
}
\alias{help.start}
\arguments{
  \item{gui}{just for compatibility}
  \item{browser}{the name of the program to be used as hypertext browser.}
#ifdef unix  
  \item{remote}{A character giving a valid URL for the
    \file{\$R\_HOME} directory on a remote location.}
#endif
}
\description{
  Start the hypertext (currently HTML) version of \R's online documentation.
}
\details{
#ifdef unix
  All the packages in the current value of \code{.lib.loc} are linked to
  a directory \code{.R} in the user's home directory.

  The \code{browser} needs to work with a \code{"-remote openURL(..)"}
  call. Then, the hypertext \R help interface is opened in any already
  running browser or a new one if necessary.

  All future help requests are sent to the browser.
#endif
#ifdef windows
  The Windows file association mechanism is used to send the HTML file
  to a browser, launching one if necessary.

  Unlike Unix systems, running \code{help.start} does not send all
  future help requests to the browser: use \code{options(htmlhelp=TRUE)}
  to set that.
#endif
}

#ifdef unix
\note{
  As a side-effect, the variable \code{"help.start.has.been.run"} in the
  environment \code{"package:base"} is set to true.
}
#endif

\seealso{
  \code{\link{help}()} for on- and off-line help in ASCII/Editor or
  PostScript format.
}
\examples{
\dontrun{help.start()}
}
\keyword{documentation}
