% File src/library/utils/man/unix/BATCH.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

% Differs from the Unix version in mentioning --no-readline and not
% running in the background.

\name{BATCH}
\alias{BATCH}
\title{Batch Execution of R}
\description{Run \R non-interactively with input from \code{infile} and
  send output (stdout/stderr) to another file.}
\usage{
\special{R CMD BATCH [options] infile [outfile]}
}
\arguments{
  \item{infile}{the name of a file with \R code to be executed.}
  \item{options}{a list of \R command line options, e.g., for setting the
    amount of memory available and controlling the load/save process.
    If \code{infile} starts with a \samp{-}, use \option{--} as the final
    option.
    The default options are \option{--restore --save --no-readline}.}
  \item{outfile}{the name of a file to which to write output.  If not
    given, the name used is that of \code{infile}, with a possible
    \file{.R} extension stripped, and \file{.Rout} appended.}
}
\details{
  Use \command{R CMD BATCH --help} to be reminded of the usage.

  By default, the input commands are printed along with the output.  To
  suppress this behavior, add \code{options(echo = FALSE)} at the
  beginning of \code{infile}, or use option \option{--slave}.

  The \code{infile} can have end of line marked by LF or CRLF (but not
  just CR), and files with an incomplete last line (missing end of line
  (EOL) mark) are processed correctly.

  A final expression \samp{proc.time()} will be executed after the input
  script unless the latter calls \code{\link{q}(runLast = FALSE)} or is aborted.
  This can be suppressed by the option \option{--no-timing}.

  Additional options can be set by the environment variable
  \env{R_BATCH_OPTIONS}: these come after
  \option{--restore --save --no-readline} and before any options given
  on the command line.
}
\note{
  Unlike \command{Splus BATCH}, this does not run the \R process in the
  background.  In most shells,
  \command{R CMD BATCH [options] infile [outfile] &}
  will do so.
}
\keyword{utilities}
