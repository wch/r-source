% File src/library/tools/man/startDynamicHelp.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2015 R Core Team
% Distributed under GPL 2 or later

\name{startDynamicHelp}
\alias{startDynamicHelp}
\alias{help.ports}
\alias{R_DISABLE_HTTPD}

\title{Start the Dynamic HTML Help System}
\description{
  This function starts the internal help server, so that HTML help pages
  are rendered when requested.
}
\usage{
startDynamicHelp(start = TRUE)
}
\arguments{
  \item{start}{logical: whether to start or shut down the dynamic help
    system.  If \code{NA}, the server is started if not already running.}
}
\details{
  This function starts the internal HTTP server, which runs on the
  loopback interface (\code{127.0.0.1}).  If \code{options("help.ports")}
  is set to a vector of non-zero integer values, \code{startDynamicHelp} will try
  those ports in order; otherwise, it tries up to 10 random ports to
  find one not in use.  It can be disabled by setting the environment
  variable \env{R_DISABLE_HTTPD} to a non-empty value or \code{options("help.ports")}
  to \code{0}.

  \code{startDynamicHelp} is called by functions that need to use the
  server, so would rarely be called directly by a user.

  Note that \code{options(help_type = "html")} must be set to actually
  make use of HTML help, although it might be the default for an \R
  installation.

  If the server cannot be started or is disabled,
  \code{\link{help.start}} will be unavailable and requests for HTML
  help will give text help (with a warning).

  The browser in use does need to be able to connect to the loopback
  interface: occasionally it is set to use a proxy for HTTP on all
  interfaces, which will not work -- the solution is to add an exception
  for \code{127.0.0.1}.
}
\value{
  The chosen port number is returned invisibly (which will be \code{0}
  if the server has been stopped).
}
\seealso{
  \code{\link{help.start}} and \code{\link{help}(help_type = "html")}
  will attempt to start the HTTP server if required

  \code{\link{Rd2HTML}} is used to render the package help pages.
}
\keyword{ documentation }
