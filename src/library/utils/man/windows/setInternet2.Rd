% File src/library/utils/man/windows/setInternet2.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{setInternet2}
\Rdversion{1.1}
\alias{setInternet2}
\title{
Set or disable the use of Internet Explorer for Internet access.
}
\description{
  With the \option{--internet2} startup option, or \emph{via}
  environment variable \env{R_WIN_INTERNET2} or by calling this
  function, R will make use of the \file{internet2.dll} library for
  Internet access, which makes use of Internet Explorer functions.  This
  allows specification of proxies, etc., in the \dQuote{Internet
    Options} applet in \dQuote{Control Panel}.
}
\usage{
setInternet2(use = TRUE)
}
\arguments{
  \item{use}{Whether to use \file{internet2.dll}.  If \code{NA}, just
  return the current setting.
}
}
\value{
The previous setting is returned, invisibly if \code{use} is not \code{NA}.
}
\details{
  R does not load either \file{internet.dll} or \file{internet2.dll}
  until needed, so it is certainly safe to call this function before
  making any calls that access the Internet
  (e.g., \code{\link{download.file}}).  If one of the DLLs has already
  been loaded, this function will switch to the other one without
  unloading the first one, so there is a potential resource leak.
}
\note{
  This is only available on Windows.
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
