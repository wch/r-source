% File src/library/base/man/interactive.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{interactive}
\alias{interactive}
\title{Is R Running Interactively?}
\description{
  Return \code{TRUE} when \R is being used interactively and
  \code{FALSE} otherwise.
}
\usage{
interactive()
}
\details{
  An interactive \R session is one in which it is assumed that there is
  a human operator to interact with, so for example \R can prompt for
  corrections to incorrect input or ask what to do next or if it is OK
  to move to the next plot.

  GUI consoles will arrange to start \R in an interactive session.  When
  \R is run in a terminal (via \command{Rterm.exe} on Windows), it
  assumes that it is interactive if \file{stdin} is connected to a
  (pseudo-)terminal and not if \file{stdin} is redirected to a file or
  pipe.  Command-line options \option{--interactive} (Unix) and
  \option{--ess} (Windows, \command{Rterm.exe}) override the default
  assumption.
#ifdef unix
  (On a Unix-alike, whether the \code{readline} command-line editor is
  used is \strong{not} overridden by \option{--interactive}.)
#endif

  Embedded uses of \R can set a session to be interactive or not.

  Internally, whether a session is interactive determines
  \itemize{
    \item how some errors are handled and reported, e.g.\sspace{}see
    \code{\link{stop}} and \code{\link{options}("showWarnCalls")}.

    \item  whether one of \option{--save}, \option{--no-save} or
    \option{--vanilla} is required, and if \R ever asks whether to save the
    workspace.

    \item the choice of default graphics device launched when needed and
    by \code{\link{dev.new}}: see \code{\link{options}("device")}

    \item whether graphics devices ever ask for confirmation of a new
    page.
  }
  In addition, \R's own \R code makes use of \code{interactive()}: for
  example \code{\link{help}}, \code{\link{debugger}} and
  \code{\link{install.packages}} do.
}
\note{
  This is a \link{primitive} function.
}
\seealso{
  \code{\link{source}},
  \code{\link{.First}}
}
\examples{
 .First <- function() if(interactive()) x11()
}
\keyword{environment}
\keyword{programming}
