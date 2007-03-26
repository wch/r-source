\name{shell}
\alias{shell}
\title{Invoke a System Command, using a Shell}

\usage{
shell(cmd, shell, flag = "/c", intern = FALSE, wait = TRUE,
      translate = FALSE, mustWork = FALSE, ...)
}

\arguments{
  \item{cmd}{the system command to be invoked, as a string.}
  \item{shell}{a string giving the name of the shell to be used, or
    \code{NULL} (no shell).  If missing, a suitable shell is chosen:
    see \sQuote{Details}.}
  \item{flag}{the switch to run a command under the shell.  If the
    shell is \code{bash} or \code{tcsh} the default is changed to
    \code{"-c"}.}
  \item{intern}{a logical, indicates whether to make the output of the
    command an \R object.}
  \item{wait}{should the \R interpreter wait for the command to finish?
    The default is to wait, and the interpreter will always wait if
    \code{intern = TRUE}.}
  \item{translate}{If \code{TRUE}, \code{"/"} in \code{cmd} is
    translated to \code{"\\"}.}
  \item{mustWork}{a logical; if \code{TRUE} failure to run the command
    will give an \R error.}
  \item{\dots}{additional arguments to \code{\link{system}}.}
}

\description{
  \code{shell} runs the command specified by \code{cmd}, usually under
  a shell.
}

\value{
  If \code{intern=TRUE}, a character vector giving the output of the
  command, one line per character string, or an error message if the
  command could not be run.

  If \code{intern=FALSE}, the return value is an error code, given the
  invisible attribute (so needs to be printed explicitly).  If the
  command could not be run for any reason, the value is \code{-1} and
  an \R warning is generated. Otherwise if \code{wait=FALSE} the value
  is the error code returned by the command, and if \code{wait=TRUE}
  it is the zero (the conventional success value),

  If \code{intern=FALSE} and \code{wait=TRUE} (the defaults) the text
  output from a command that is a console application will appear in
  the \R console (\code{Rgui}) or the window running \R (\code{Rterm}).
}

\details{
  If no \code{shell} is specified, the environment variables
  \env{R\_SHELL}, \env{SHELL} and \env{COMSPEC} are tried in turn:
  \env{COMSPEC} should always succeed. Using \code{shell=NULL}
  invokes the command \code{cmd} directly, in which case an extension
  of \code{.exe} is assumed. It is possible to use batch files
  directly if their extension is given: Windows (rather than \R) then
  chooses a shell.
    
  See \code{\link{system}} for fuller details: \code{shell} is a more
  user-friendly wrapper for \code{system}.  To make use of Windows
  file associations, use \code{\link{shell.exec}}.
}

\seealso{\code{\link{system}}, \code{\link{shell.exec}}}

\keyword{utilities}
