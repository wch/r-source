% File src/library/utils/man/windows/setInternet2.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{setInternet2}
\Rdversion{1.1}
\alias{setInternet2}
\alias{R_WIN_INTERNET2}

\title{
Set or disable the use of Windows internal functions for Internet access.
}
\description{
  By default R will make use
  of the Windows functions underlying Internet Explorer (the \code{"wininet"}
  functions).  This allows
  specification of proxies, etc., in the \dQuote{Internet Options}
  applet in \dQuote{Control Panel}.  This will allow the use of
  \samp{https://} URLs and follow HTTP redirections.  This function
  allows substitution of lower level access methods via \code{setInternet2(FALSE)}.
}

\usage{
setInternet2(use = TRUE)
}
\arguments{
  \item{use}{logical: whether to use the Windows internet functions.  If
    \code{NA}, just return the current setting.}
}
\value{
  The previous setting is returned, invisibly if \code{use} is not \code{NA}.
}
\note{
  This is only available on Windows.

  Prior to \R 3.2.0 \code{setInternet2(TRUE)} loaded a new DLL:
  nowadays both versions of the functions are contained in 
  \file{internet.dll}.
  
  Prior to \R 3.2.2, the default was \code{setInternet2(FALSE)},
  unless command line option \option{--internet2} was used or
  environment variable \env{R_WIN_INTERNET2} was set. (The option and
  environment variable are now ignored.)  With this setting the 
  \code{"internal"} method of \code{\link{download.file}} and 
  \code{\link{url}} uses internal functions for \samp{http://} access, and 
  \samp{https://} URLs are not supported.  The current default
  is equivalent to setting the method to \code{"wininet"}.
}
\author{
Duncan Murdoch
}
\seealso{
  \code{\link{download.file}}, \code{\link{url}}: the method can be
  selected for each \code{url} connection via its \code{method} argument.
}
\examples{
setInternet2(NA)
}
\keyword{ utilities }
