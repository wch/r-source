% File src/library/base/man/system.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{system2}
\alias{system2}
\title{Invoke a System Command}
\description{
  \code{system2} invokes the OS command specified by \code{command}.
}
\usage{
system2(command, args = character(),
        stdout = "", stderr = "", stdin = "", input = NULL,
        env = character(), wait = TRUE,
        minimized = FALSE, invisible = TRUE)
}
\arguments{
  \item{command}{the system command to be invoked, as a character string.}
  \item{args}{a character vector of arguments to \command{command}.}
  \item{stdout, stderr}{where output to \file{stdout} or
    \file{stderr} should be sent.  Possible values are \code{""}, to the \R
    console (the default), \code{NULL} or \code{FALSE} (discard output),
    \code{TRUE} (capture the output in a character vector) or a
    character string naming a file.}
  \item{stdin}{should input be diverted?  \code{""} means the default,
    alternatively a character string naming a file.  Ignored
    if \code{input} is supplied.}
  \item{input}{if a character vector is supplied, this is copied one
    string per line to a temporary file, and the standard input of
    \code{command} is redirected to the file.}
  \item{env}{character vector of name=value strings to set environment
    variables.}
  \item{wait}{a logical (not \code{NA}) indicating whether the \R
    interpreter should wait for the command to finish, or run it
    asynchronously.  This will be ignored (and the interpreter will
    always wait) if \code{stdout = TRUE}.}
#ifdef unix
  \item{minimized, invisible}{arguments that are accepted on Windows but
    ignored on this platform, with a warning.}
#endif
#ifdef windows
  \item{minimized}{logical (not \code{NA}), indicates whether the
    command window should be displayed initially as a minimized window.}
  \item{invisible}{logical (not \code{NA}), indicates whether the
    command window should be visible on the screen.}
#endif
}
\details{
  Unlike \code{\link{system}}, \code{command} is always quoted by
  \code{\link{shQuote}}, so it must be a single command without arguments.

  For details of how \code{command} is found see \code{\link{system}}.

  On Windows, \code{env} is only supported for commands such as
  \command{R} and \command{make} which accept environment variables on
  their command line.

  Some Unix commands (such as some implementations of \code{ls}) change
  their output if they consider it to be piped or redirected:
  \code{stdout = TRUE} uses a pipe whereas \code{stdout =
  "some_file_name"} uses redirection.

  Because of the way it is implemented, on a Unix-alike \code{stderr =
    TRUE} implies \code{stdout = TRUE}: a warning is given if this is
  not what was specified.
}
%% We use popen, and that pipes stdout only

\value{
  If \code{stdout = TRUE} or \code{stderr = TRUE}, a character vector
  giving the output of the command, one line per character string.
  (Output lines of more than 8095 bytes will be split.)  If the command
  could not be run an \R error is generated.  If \code{command} runs but
  gives a non-zero exit status this will be reported with a warning and
  in the attribute \code{"status"} of the result: an attribute
  \code{"errmsg"} may also be available.

  In other cases, the return value is an error code (\code{0}
  for success), given the \link{invisible} attribute (so needs to be printed
  explicitly).  If the command could not be run for any reason, the
  value is \code{127}.  Otherwise if \code{wait = TRUE} the value is the
  exit status returned by the command, and if \code{wait = FALSE} it is
  \code{0} (the conventional success value).
#ifdef windows
  Some Windows commands return out-of-range status values
  (e.g., \code{-1}) and so only the bottom 16 bits of the value are used.
#endif
}

\note{
  \code{system2} is a more portable and flexible interface than
  \code{\link{system}}, introduced in \R 2.12.0.  It allows redirection
  of output without needing to invoke a shell on Windows, a portable way
  to set environment variables for the execution of \code{command}, and
  finer control over the redirection of \code{stdout} and \code{stderr}.
  Conversely, \code{system} (and \code{shell} on Windows) allows the
  invocation of arbitrary command lines.

  There is no guarantee that if \code{stdout} and \code{stderr} are both
  \code{TRUE} or the same file that the two streams will be interleaved
  in order.  This depends on both the buffering used by the command and
  the OS.
}

\seealso{
  \code{\link{system}}.
#ifdef windows

  \code{\link{shell}} and \code{\link{shell.exec}}.
#endif
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
