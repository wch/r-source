% File src/library/tools/man/startDynamicHelp.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{startDynamicHelp}
\alias{startDynamicHelp}
\title{
Start the dynamic HTML help system
}
\description{
This function starts the internal help server, so that HTML help pages
are rendered when requested.
}
\usage{
startDynamicHelp()
}
\details{
This function starts the internal HTTPD server.  It attempts to start it
on port 8080, but will try subsequent ports if that one is in use.  It also sets
\code{option(htmlhelp=TRUE)}.
}
\value{
The chosen port number is returned invisibly.
}
\seealso{
\code{\link{Rd2HTML}} is used to render the man pages.
}
\keyword{ documentation }
