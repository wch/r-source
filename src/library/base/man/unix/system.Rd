\name{system}
\alias{system}
\title{Invoke a System Command}
\description{
  \code{system} invokes the OS command specified by \code{command}.
}
\usage{
system(command, intern = FALSE, ignore.stderr = FALSE,
       wait = TRUE, input = NULL, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)
}
\arguments{
  \item{command}{the system command to be invoked, as a string.}
  \item{intern}{a logical (not \code{NA}) which indicates whether to 
    make the output of the command an \R object.  Not available unless
    \code{popen} is supported on the platform.}
  \item{ignore.stderr}{a logical indicating whether error messages (written
    to \file{stderr}) should be ignored.}
  \item{wait}{a logical indicating whether \R should wait for the
    command to finish, or run it asynchronously (when \code{intern} will
    not be useful.}
  \item{input}{if a character vector is supplied, this is copied one
    string per line to a temporary file, and the standard input of
    \code{command} is redirected to the file.}
  \item{show.output.on.console, minimized, invisible}{arguments
    that are accepted on other platforms but ignored on this one, with a
    warning.}
}
\value{
  If \code{intern = TRUE}, a character vector giving the output of the
  command, one line per character string.  If the command could not be
  run or gives an error this will be reported on the shell's
  \file{stderr} (unless \code{popen} is not supported, when there is an \R
  error).  (Output lines of more than 8095 characters will be split.)

  If \code{intern = FALSE}, the return value is a system error code
  (\code{0} for success).
}
\details{
  On Unix-alikes \code{system} runs the system shell, and so both the
  command in \code{command} and any filepath arguments need to be
  enclosed in quotes (see \code{\link{shQuote}}).
  
  If \code{intern} is \code{TRUE} then \code{popen} is used to invoke the
  command and the output collected, line by line, into an \R
  \code{\link{character}} vector which is returned as the value of
  \code{system}.  Output lines of more than 8095 characters will be split.

  If \code{intern} is \code{FALSE} then the C function \code{system}
  is used to invoke the command and the value returned by \code{system}
  is the exit status of this function.

  Error messages written to \file{stderr} will be sent by the shell to
  the terminal unless \code{ignore.stderr = TRUE}.  They can be captured
  (in the most likely shells) by
  \preformatted{
    system("some command 2>&1", intern=TRUE)
  }
  What happens to output sent to \file{stdout} and \file{stderr} if
  \code{intern = FALSE} is interface-specific, and it is unsafe to
  assume that such messages will appear on the console (they do on the
  MacOS X console but not on the \pkg{gnomeGUI} console, for example).
  
  \code{wait} is implemented by appending \code{&} to the command: this
  is shell-dependent, but required by POSIX and so widely supported.

  The Windows version of \R has additional arguments accepted for
  compatibility.

  The ordering of arguments after the first two has changed from time to
  time: it is recommended to name all arguments after the first.
}
\seealso{
  \code{\link{.Platform}} for platform specific variables.
}
\examples{
# list all files in the current directory using the -F flag
\dontrun{system("ls -F")}

# t1 is a character vector, each one
# representing a separate line of output from who
# (if the platform has popen and who)
t1 <- try(system("who", intern = TRUE))

try(system("ls fizzlipuzzli", intern = TRUE, ignore.stderr = TRUE))
# empty since file doesn't exist
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
