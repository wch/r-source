\name{help.start}
\title{Hypertext Documentation}
\usage{
#ifdef unix  
help.start(gui = "irrelevant", browser = getOption("browser"), remote = NULL)
#endif
#ifdef windows  
help.start(update = TRUE, gui = "irrelevant", browser = getOption("browser"))
#endif
}
\alias{help.start}
\arguments{
  \item{gui}{just for compatibility with S-PLUS.}
  \item{browser}{the name of the program to be used as hypertext
    browser. It should be in the PATH, or a full path specified.
  }
#ifdef unix  
  \item{remote}{A character giving a valid URL for the
    \file{\$R\_HOME} directory on a remote location.}
#endif
#ifdef windows  
  \item{update}{logical: should this attempt to update the indices to
    reflect the currently installed packages.}
#endif
}
\description{
  Start the hypertext (currently HTML) version of \R's online documentation.
}
\details{
#ifdef unix
  All the packages in the known library trees are linked to a directory
  \code{.R} in the user's home directory.

  The \code{browser} needs to work with a \code{"-remote openURL(...)"}
  call. Then, the hypertext \R help interface is opened in any already
  running browser or a new one if necessary.  Browsers that support this
  interface include Netscape 4.x, 6.2.x (but not 6.0/1), Opera 5/6 and
  Mozilla >= 0.9.5.

  If the browser given by the \code{browser} argument is different from
  the default browser as specified by \code{options("browser")}, the
  default is changed to the given browser so that it gets used for all
  future help requests.
#endif
#ifdef windows
  The Windows file association mechanism is used to send the HTML file
  to a browser, launching one if necessary.

  Unlike Unix systems, running \code{help.start} does not send all
  future help requests to the browser: use \code{options(htmlhelp=TRUE)}
  to set that.

  Alternatively, a specific browser can be started if \code{browser} is
  specified, but \code{help} will still use the file association mechanism.

  \code{help.start} and the Java-based search engine have been tested
  under Internet Explorer 6, Netscape 6.2.2, Opera 6.01 and Mozilla 0.9.9.
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
\dontrun{help.start()
#ifdef windows
help.start(browser="C:\\Program Files\\Internet Explorer\\IEXPLORE.EXE")
help.start(browser="C:/Program Files/Netscape/Netscape 6/netscp6.exe")
help.start(browser="C:/Program Files/Opera/Opera.exe")
#endif
}}
\keyword{documentation}
