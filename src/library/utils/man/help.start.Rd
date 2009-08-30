% File src/library/utils/man/help.start.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

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
           remote = NULL, searchEngine = FALSE)
#endif
#ifdef windows  
help.start(update = TRUE, gui = "irrelevant",
           browser = getOption("browser"), searchEngine = FALSE)
#endif
}
\arguments{
  \item{gui}{just for compatibility with S-PLUS.}
  \item{browser}{the name of the program to be used as hypertext
    browser. It should be in the \env{PATH}, or a full path specified.
    Alternatively, it can be an \R function which will be called with a
    URL as its only argument.
#ifdef windows
    This option is normally unset on Windows, when the file-association
    mechanism will be used.
#endif
  }
#ifdef unix  
  \item{remote}{A character giving a valid URL for the
    \file{\var{\link[base:Rhome]{R_HOME}}} directory on a remote location.}
#endif
#ifdef windows  
  \item{update}{logical: should this attempt to update the indices to
    reflect the currently installed packages.}
#endif
  \item{searchEngine}{logical: if true go directly to the search engine
    page.  A workaround for some Firefox 3 versions (see \sQuote{Notes}).}
}
\details{
#ifdef unix
  All the packages in the known library trees are linked to directory
  \file{.R} in the per-session temporary directory.  The links are
  re-made each time \code{\link{make.packages.html}} or \code{help.start}
  is run, which should be done after packages are installed, updated or
  removed.

  If the browser named by the \code{browser} argument is different from
  the default browser as specified by \code{options("browser")}, the
  default is changed to the given browser so that it gets used for all
  future help requests.  (This only holds when \code{browser} is a name,
  not a function.)
#endif
#ifdef windows
  \code{help.start} and the Java-based search engine have been tested
  under Internet Explorer, Firefox, Chrome, Opera and Mozilla 1.7.
  Users of IE may need to allow \sQuote{active content} from the
  \sQuote{information bar} before the search engine can be used.
#endif
}
\note{
  There is a Java-based search facility available from the HTML page
  that \code{help.start} brings up.  Should this not work, please
  consult the \sQuote{R Installation and Administration} manual which is
  linked from that page.

  Note to users of Firefox 3.0 to 3.0.6: the search results have links
  that are resolved incorrectly by that browser if starting from the
  normal HTML index page.  The workaround is to start with
  \code{help.start(searchEngine = TRUE)}.  This was fixed in Firefox 3.0.7.
}

\seealso{
  \code{\link{help}()} for on- and off-line help in other formats.

  \code{\link{browseURL}} for how the help file is displayed.

  \code{\link{RSiteSearch}} to access an on-line search of \R resources.
}
\examples{
\dontrun{help.start()
#ifdef windows
help.start(browser="C:\\\\Program Files\\\\Internet Explorer\\\\IEXPLORE.EXE")
help.start(browser="C:/Program Files/Opera/Opera.exe")
help.start(browser="C:/Program Files/Mozilla Firefox/firefox.exe")
#endif
}}
\keyword{documentation}
