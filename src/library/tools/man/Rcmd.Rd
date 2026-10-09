% File src/library/tools/man/Rcmd.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016-2026 R Core Team
% Distributed under GPL 2 or later

\name{Rcmd}
\alias{Rcmd}
\title{\command{R CMD} Interface}
\description{
  Invoke \command{R CMD} tools, such as \file{\link{build}} and
  \file{\link{SHLIB}}, from within \R.
}
\usage{
Rcmd(args, ...)
}
\arguments{
  \item{args}{a character vector of arguments to \command{R CMD}.}
  \item{\dots}{arguments to be passed to \code{\link{system2}}.}
}
\value{
  See section \dQuote{Value} in \code{\link{system2}}.
}
\details{
  Provides a portable convenience interface to the \command{R CMD}
  mechanism by invoking the corresponding system commands (using the
  version of \R currently used) via \code{\link{system2}}.
  However, passing \code{env} is currently not supported on Windows.%\PR{16311}
}
\seealso{
  \manual{R-intro}{Invoking R from the command line},
  or \command{R --help} in a terminal,
  for a list of available commands.
}
\examples{\donttest{
## query the default C compiler command for this build of R
Rcmd(c("config", "CC"))
}}
\keyword{utilities}
