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
    make the output of the command an \R object.
#ifdef unix
    Not available unless \code{popen} is supported on the platform.
#endif
  }
  \item{ignore.stderr}{a logical indicating whether error messages written
    to \file{stderr} should be ignored.}
  \item{wait}{a logical indicating whether the \R interpreter should
    wait for the command to finish, or run it asynchronously.
    This will be ignored (and the interpreter will always wait) if
    \code{intern = TRUE}.}
  \item{input}{if a character vector is supplied, this is copied one
    string per line to a temporary file, and the standard input of
    \code{command} is redirected to the file.}
#ifdef unix
  \item{show.output.on.console, minimized, invisible}{arguments
    that are accepted on other platforms but ignored on this one, with a
    warning.}
#endif
#ifdef windows
  \item{show.output.on.console}{a logical, indicates whether to capture
    the output of the command and show it on the \R console (not used
    by \code{Rterm}, which captures the output unless \code{wait}
    is false).}
  \item{minimized}{a logical, indicates whether the command window
    should be initially displayed as a minimized window.}
  \item{invisible}{a logical, indicates whether the command window
    should be visible on the screen.}
#endif
}
\details{
  \code{command} is parsed as a command plus arguments separated by spaces.
  So if the path to the command (or a filepath argument) contains
  spaces, it must be quoted e.g. by \code{\link{shQuote}}.
#ifdef windows
  Only double quotes are allowed on Windows: see the examples. (Note: a
  Windows path name cannot contain a double quote, so we do not need to
  worry about escaping embedded quotes.)
#endif

  How the command is run differs by platform: Unix-alikes use a shell
  (\file{/bin/sh} by default), and Windows executes the command directly
  (extensions \code{.exe}, \code{.com}) or as a batch file (extensions
  \code{.cmd} and \code{.bat}).
#ifdef windows
  (These extensions are tried in turn if none is supplied.)
  This means that redirection, pipes, \dots cannot be used: see
  \code{\link{shell}}. (To use DOS internal commands use
  \code{paste(Sys.getenv("COMSPEC"),"/c", cmd)}.)
  
  The search path for \code{command} may be system-dependent: it will
  include the \R \code{bin} directory, the working directory and the
  Windows system directories before \code{PATH}.
#endif

#ifdef unix
  If \code{intern} is \code{TRUE} then \code{popen} is used to invoke the
  command and the output collected, line by line, into an \R
  \code{\link{character}} vector.  If \code{intern} is \code{FALSE} then
  the C function \code{system} is used to invoke the command.
#endif

  The ordering of arguments after the first two has changed from time to
  time: it is recommended to name all arguments after the first.
}

#ifdef unix
\section{Stdout and stderr}{
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
}
\note{
  \code{wait} is implemented by appending \code{&} to the command: this
  is shell-dependent, but required by POSIX and so widely supported.
}
#endif

#ifdef windows
\section{Interaction with the command}{
  Precisely what is seen by the user depends on whether \code{Rgui} or
  \code{Rterm} is being used.  For \code{Rgui} a new \sQuote{console}
  will always be used, so a commands window will appear for the duration
  of console applications unless \code{invisible} is true (which it is
  by default). For \code{Rterm} a separate commands window will appear
  for console applications only if \code{wait = FALSE} and
  \code{invisible = FALSE}.

  If the \R process is waiting for output from the command it is
  possible to interrupt the running command from the keyboard or
  \code{Rgui} menu: this should at least return control to the \R
  console.  Otherwise it it not possible to interrupt the running
  command.

  Do not try to run console applications that require user
  input from \code{Rgui} setting \code{intern = TRUE} and/or
  \code{show.output.on.console = TRUE}.  They will not work,
  may hang and then will probably hang \code{Rgui} too.
}
#endif

\value{
  If \code{intern = TRUE}, a character vector giving the output of the
  command, one line per character string.  (Output lines of more than
  8095 characters will be split.) If the command could not be run or
  gives an error
#ifdef unix
  this will be reported on the shell's \file{stderr} (unless
  \code{popen} is not supported, when there is an \R error).
#endif
#ifdef windows
  an \R error is generated.
#endif
#ifdef windows
  This also captures \code{stderr} on the RGui console unless
  \code{ignore.stderr = TRUE}.
#endif

  If \code{intern = FALSE}, the return value is an error code(\code{0}
  for success), given the invisible attribute (so needs to be printed
  explicitly).  If the command could not be run for any reason, the
  value is
#ifdef unix
  \code{256*127 = 52512}.
#endif
#ifdef windows
  \code{-1}. (An \R warning is also generated.)
#endif
  Otherwise if \code{wait = TRUE} the value is
#ifdef unix
  256 times
#endif
  the error code returned by the command, and if \code{wait = FALSE} it
  is \code{0} (the conventional success value).
#ifdef windows

  If \code{intern = FALSE} and \code{show.output.on.console = TRUE}
  the \file{stdout} and \file{stderr} (unless \code{ignore.stderr =
    TRUE}) output from a command that is a console application will
  appear in the \R console (\code{Rgui}) or the window running \R
  (\code{Rterm}).
#endif
}

\seealso{
#ifdef windows
  \code{\link{shell}} or \code{\link{shell.exec}} for a less raw
  interface.

#endif
  \code{\link{.Platform}} for platform-specific variables.
}
#ifdef unix
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
#endif
#ifdef windows
\examples{
# launch an editor, wait for it to quit
\dontrun{system("notepad myfile.txt")}
# launch your favourite shell:
\dontrun{system(Sys.getenv("COMSPEC"))}
\dontrun{
## note the two sets of quotes here:
system(paste('"c:/Program Files/Mozilla Firefox/firefox.exe"',
             '-url cran.r-project.org'), wait = FALSE)}
}
#endif
\keyword{interface}
\keyword{file}
\keyword{utilities}
