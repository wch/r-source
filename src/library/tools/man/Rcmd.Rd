% File src/library/tools/man/Rcmd.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2016 R Core Team
% Distributed under GPL 2 or later

\name{Rcmd}
\alias{Rcmd}
\title{R CMD Interface}
\description{
  Invoke \command{R CMD} tools from within \R.
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
}
\keyword{utilities}
