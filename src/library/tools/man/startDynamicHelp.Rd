% File src/library/tools/man/startDynamicHelp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{startDynamicHelp}
\alias{startDynamicHelp}
\title{Start the dynamic HTML help system}
\description{
  This function starts the internal help server, so that HTML help pages
  are rendered when requested.
}
\usage{
startDynamicHelp(start=TRUE)
}
\arguments{
  \item{start}{logical: whether to start or shut down the dynamic help
    system.}
}
\details{
  This function starts the internal HTTPD server.  It tries up to 10
  ports to find one not in use.

  Note that \code{option(htmlhelp=TRUE)} must be set to actually make use
  of dynamic HTML help.
}
\value{
  The chosen port number is returned invisibly (which will be \code{0}
  if the server has been stopped).
}
\seealso{
  \code{\link{Rd2HTML}} is used to render the package help pages.
}
\keyword{ documentation }
