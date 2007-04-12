\name{help.start}
\title{Hypertext Documentation}
\alias{help.start}
\description{
  Start the hypertext (currently HTML) version of \R's online
  documentation.
}
\usage{
#ifdef unix  
help.start(gui = "irrelevant", browser = getOption("browser"),
           remote = NULL)
#endif
#ifdef windows  
help.start(update = TRUE, gui = "irrelevant",
           browser = getOption("browser"))
#endif
}
\arguments{
  \item{gui}{just for compatibility with S-PLUS.}
  \item{browser}{the name of the program to be used as hypertext
    browser. It should be in the PATH, or a full path specified.
  }
#ifdef unix  
  \item{remote}{A character giving a valid URL for the \file{\$R\_HOME}
    directory on a remote location.}
#endif
#ifdef windows  
  \item{update}{logical: should this attempt to update the indices to
    reflect the currently installed packages.}
#endif
}
\details{
#ifdef unix
  All the packages in the known library trees are linked to directory
  \file{.R} in the per-session temporary directory.  The links are
  re-made each time \code{help.start} is run, which should be done
  after packages are installed, updated or removed.

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
  under Internet Explorer 6/7, Netscape 7.1, Opera 7.54,
  Mozilla 1.6/1.7 and Firefox 1.0/1.5/2.0.  Users of IE
  will probably need to allow `active content' from the
  \sQuote{information bar} before the search engine can be used.
#endif
}
\note{
  There is a Java-based search facility available from the HTML page
  that \code{help.start} brings up.  Should this not work, please
  consult the \sQuote{R Installation and Administration} manual which is
  linked from that page.
}

\seealso{
  \code{\link{help}()} for on- and off-line help in ASCII/Editor or
  PostScript format.

  \code{\link{browseURL}} for how the help file is displayed.

  \code{\link{RSiteSearch}} to access an on-line search of \R resources.
}
\examples{
\dontrun{help.start()
#ifdef windows
help.start(browser="C:\\\\Program Files\\\\Internet Explorer\\\\IEXPLORE.EXE")
help.start(browser="C:/Program Files/Netscape/Netscape/netscp.exe")
help.start(browser="C:/Program Files/Opera/Opera.exe")
help.start(browser="C:/Program Files/mozilla.org/Mozilla/mozilla.exe")
help.start(browser="C:/Program Files/Mozilla Firefox/firefox.exe")
#endif
}}
\keyword{documentation}
