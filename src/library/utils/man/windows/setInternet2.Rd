% File src/library/utils/man/windows/setInternet2.Rd
% Part of the R package, http://www.R-project.org
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
  With the \option{--internet2} startup option, or \emph{via}
  environment variable \env{R_WIN_INTERNET2} or by calling this
  function, R will make use of the Windows functions underlying
  Internet Explorer.  This allows specification of proxies, etc., in the
  \dQuote{Internet Options} applet in \dQuote{Control Panel}.  Unlike
  the standard internal functions it will allow the use of
  \samp{https://} URLs and follow HTTP redirections.
}
\usage{
setInternet2(use = TRUE)
}
\arguments{
  \item{use}{lgical: whether to use the Windows internet functions.  If
    \code{NA}, just return the current setting.}
}
\value{
  The previous setting is returned, invisibly if \code{use} is not \code{NA}.
}
\note{
  This is only available on Windows.

  Prior to \R 3.2.0 this loaded a new DLL: nowadays both versions of
  the functions are contained in \file{internet.dll}.
}
\author{
Duncan Murdoch
}
\seealso{
\code{\link{download.file}}, \code{\link{url}}
}
\examples{
setInternet2(NA)
}
\keyword{ utilities }
