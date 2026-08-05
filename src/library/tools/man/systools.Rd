% File src/library/tools/man/systools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{systools}
\alias{system_with_capture}
\title{Invoke a System Command}
\description{
  Run \code{\link{system2}()} separately capturing status, stdout and
  stderr.
}
\usage{
system_with_capture(command, args = character(), env = character(),
                    stdin = "", input = NULL, timeout = 0)
}
\arguments{
  \item{command, args, env, stdin, input, timeout}{as for
    \code{\link{system2}}.}
}
\value{
  A list with elements \code{status}, \code{stdout} and \code{stderr}
  giving the status of and stdout and stderr obtained from running the
  given system command.
}
\examples{
## Re-using the example from ?system:
## Both the error message and status code are OS-dependent
\dontdiff{system_with_capture("ls", "fizzlipuzzli")}
}
