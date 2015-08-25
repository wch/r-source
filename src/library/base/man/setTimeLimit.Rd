% File src/library/base/man/setTimeLimit.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{setTimeLimit}
\alias{setTimeLimit}
\alias{setSessionTimeLimit}
\title{Set CPU and/or Elapsed Time Limits}
\description{
  Functions to set CPU and/or elapsed time limits for top-level
  computations or the current session.
}
\usage{
setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)

setSessionTimeLimit(cpu = Inf, elapsed = Inf)
}
\arguments{
  \item{cpu}{double.  Limit on total cpu time.}
  \item{elapsed}{double.  Limit on elapsed time.}
  \item{transient}{logical.  If \code{TRUE}, the limits apply only to
    the rest of the current computation.}
}
\details{
  \code{setTimeLimit} sets limits which apply to each top-level
  computation, that is a command line (including any continuation lines)
  entered at the console or from a file.  If it is called from within a
  computation the limits apply to the rest of the computation and
  (unless \code{transient = TRUE}) to subsequent top-level computations.

  \code{setSessionTimeLimit} sets limits for the rest of the
  session.  Once a session limit is reached it is reset to \code{Inf}.

  Setting any limit has a small overhead -- well under 1\% on the
  systems measured.

  Time limits are checked whenever a user interrupt could occur.
  This will happen frequently in \R code and during \code{\link{Sys.sleep}},
  but only at points in compiled C and Fortran code identified by the
  code author.
#ifdef unix

  \sQuote{Total cpu time} includes that used by child processes where
  the latter is reported.
#endif
}
\keyword{ utilities }
