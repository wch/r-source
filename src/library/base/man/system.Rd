% File src/library/base/man/system.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{system}
\alias{system}
#ifdef unix
\alias{shell}
#endif
\title{Invoke a System Command}
\description{
  \code{system} invokes the OS command specified by \code{command}.
}
\usage{
system(command, intern = FALSE,
       ignore.stdout = FALSE, ignore.stderr = FALSE,
       wait = TRUE, input = NULL, show.output.on.console = TRUE,
       minimized = FALSE, invisible = TRUE)
}
\arguments{
  \item{command}{the system command to be invoked, as a character string.}
  \item{intern}{a logical (not \code{NA}) which indicates whether to
    capture the output of the command as an \R character vector.}
  \item{ignore.stdout, ignore.stderr}{a logical (not \code{NA})
    indicating whether messages written to \file{stdout} or
    \file{stderr} should be ignored.}
  \item{wait}{a logical (not \code{NA}) indicating whether the \R
    interpreter should wait for the command to finish, or run it
    asynchronously.  This will be ignored (and the interpreter will
    always wait) if \code{intern = TRUE}.}
  \item{input}{if a character vector is supplied, this is copied one
    string per line to a temporary file, and the standard input of
    \code{command} is redirected to the file.}
#ifdef unix
  \item{show.output.on.console, minimized, invisible}{arguments
    that are accepted on Windows but ignored on this platform, with a
    warning.}
#endif
#ifdef windows
  \item{show.output.on.console}{logical (not \code{NA}), indicates
    whether to capture the output of the command and show it on the \R
    console (not used by \code{Rterm}, which shows the output in the
    terminal unless \code{wait} is false).}
  \item{minimized}{logical (not \code{NA}), indicates whether a
    command window should be displayed initially as a minimized window.}
  \item{invisible}{logical (not \code{NA}), indicates whether a
    command window should be visible on the screen.}
#endif
}
\details{
  \code{command} is parsed as a command plus arguments separated by spaces.
  So if the path to the command (or an argument) contains
  spaces, it must be quoted e.g. by \code{\link{shQuote}}.
#ifdef windows
  Only double quotes are allowed on Windows: see the examples.  (Note: a
  Windows path name cannot contain a double quote, so we do not need to
  worry about escaping embedded quotes.)

  \code{command} must be an executable (extensions \file{.exe},
  \file{.com}) or a batch file (extensions \file{.cmd} and \file{.bat}):
  these extensions are tried in turn if none is supplied.)  This means
  that redirection, pipes, DOS internal commands, \dots cannot be used:
  see \code{\link{shell}} if you want to pass a shell command-line.

  The search path for \code{command} may be system-dependent: it will
  include the \R \file{bin} directory, the working directory and the
  Windows system directories before \env{PATH}.
#endif
#ifdef unix
  Unix-alikes pass the command line to a shell (normally \file{/bin/sh},
  and POSIX requires that shell), so \code{command} can be anything the
  shell regards as executable, including shell scripts, and it can
  contain multiple commands separated by \code{;}.

  On Windows, \code{system} does not use a shell and there is a separate
  function \code{shell} which passes command lines to a shell.

  If \code{intern} is \code{TRUE} then \code{popen} is used to invoke the
  command and the output collected, line by line, into an \R
  \code{\link{character}} vector.  If \code{intern} is \code{FALSE} then
  the C function \code{system} is used to invoke the command.

  \code{wait} is implemented by appending \code{&} to the command: this
  is in principle shell-dependent, but required by POSIX and so widely
  supported.
#endif

  The ordering of arguments after the first two has changed from time to
  time: it is recommended to name all arguments after the first.

  There are many pitfalls in using \code{system} to ascertain if a
  command can be run --- \code{\link{Sys.which}} is more suitable.
}

\value{
  If \code{intern = TRUE}, a character vector giving the output of the
  command, one line per character string.  (Output lines of more than
  8095 bytes will be split.)  If the command could not be run an \R
  error is generated.
#ifdef windows
  Under the \code{Rgui} console \code{intern = TRUE} also captures
  \code{stderr} unless \code{ignore.stderr = TRUE}.
#endif
  If \code{command} runs but gives a non-zero exit status this will be
  reported with a warning and in the attribute \code{"status"} of the
  result: an attribute \code{"errmsg"} may also be available

  If \code{intern = FALSE}, the return value is an error code (\code{0}
  for success), given the invisible attribute (so needs to be printed
  explicitly).  If the command could not be run for any reason, the
  value is \code{127}.  Otherwise if \code{wait = TRUE} the value is the
  exit status returned by the command, and if \code{wait = FALSE} it is
  \code{0} (the conventional success value).
#ifdef windows
  Some Windows commands return out-of-range status values
  (e.g. \code{-1}) and so only the bottom 16 bits of the value are used.

  If \code{intern = FALSE, wait = TRUE, show.output.on.console = TRUE} the
  \file{stdout} and \file{stderr} (unless \code{ignore.stdout = TRUE} or
  \code{ignore.stderr = TRUE}) output from a command that is a
  \sQuote{console application} should appear in the \R console
  (\code{Rgui}) or the window running \R (\code{Rterm}).

  Not all Windows executables properly respect redirection of output, or
  may only do so from a console application such as \code{Rterm} and not
  from \code{Rgui}: for example, \file{fc.exe} was among these in the past,
  but we have had more success recently.
#endif
}

#ifdef unix
\section{Stdout and stderr}{
  For command-line \R, error messages written to \file{stderr} will be
  sent to the terminal unless \code{ignore.stderr = TRUE}.  They can be
  captured (in the most likely shells) by
\preformatted{    system("some command 2>&1", intern = TRUE)
}

  For GUIs, what happens to output sent to \file{stdout} or
  \file{stderr} if \code{intern = FALSE} is interface-specific, and it
  is unsafe to assume that such messages will appear on a GUI console
  (they do on the Mac OS X console, but not on some others).
}
#endif

#ifdef windows
\section{Interaction with the command}{
  Precisely what is seen by the user depends on the optional parameters,
  whether \code{Rgui} or \code{Rterm} is being used, and whether a
  console command or GUI application is run by the command.

  By default nothing will be seen in either front-end until the command
  finishes and the output is displayed.

  For console commands \code{Rgui} will open a new \sQuote{console}, so
  if \code{invisible = FALSE}, a commands window will appear for the
  duration of the command.  For \code{Rterm} a separate commands window
  will appear for console applications only if \code{wait = FALSE} and
  \code{invisible = FALSE}.

  GUI applications will not display in either front-end unless
  \code{invisible} is false.

  It is possible to interrupt a running command being waited for from
  the keyboard (using the \samp{Esc} key in \code{Rgui} or \samp{Ctrl-C}
  in \code{Rterm}) or from the \code{Rgui} menu: this should at least
  return control to the \R console.  \R will attempt to shut down the
  process cleanly, but may need to force it to terminate, with the
  possibility of losing unsaved work, etc.

  Do not try to run console applications that require user
  input from \code{Rgui} setting \code{intern = TRUE} or
  \code{show.output.on.console = TRUE}.  They will not work.
}
#endif

\section{Differences between Unix and Windows}{
  How processes are launched differs fundamentally between Windows and
  Unix-alike operating systems, as do the higher-level OS functions on
  which this \R function is built.  So it should not be surprising that
  there are many differences between OSes in how \code{system} behaves.
  For the benefit of programmers, the more important ones are summarized
  in this section.

  \itemize{
    \item The most important difference is that on a Unix-alike
    \code{system} launches a shell which then runs \code{command}.  On
    Windows the command is run directly -- use \code{shell} for an
    interface which runs \code{command} \emph{via} a shell (by default
    the Windows shell \command{cmd.exe}, which has many differences from
    a POSIX shell).

    This means that it cannot be assumed that redirection or piping will
    work in \code{system} (redirection sometimes does, but we have seen
    cases where it stopped working after a Windows security patch), and
    \code{\link{system2}} (or \code{shell}) must be used on Windows.

    \item What happens to \code{stdout} and \code{stderr} when not
    captured depends on how \R is running: Windows batch commands behave
    like a Unix-alike, but from the Windows GUI they are
    generally lost.  \code{system(intern = TRUE)} captures \file{stderr}
    when run from the Windows GUI console unless \code{ignore.stderr =
      TRUE}.

    \item The behaviour on error is different in subtle ways (and has
    differed between \R versions).

    \item The quoting conventions for \code{command} differ, but
    \code{\link{shQuote}} is a portable interface.

    \item Arguments \code{show.output.on.console}, \code{minimized},
    \code{invisible} only do something on Windows (and are most relevant
    to \command{Rgui} there).
  }
}
\seealso{
  \code{\link{system2}}.
  
#ifdef windows
  \code{\link{shell}} or \code{\link{shell.exec}} for a less raw
  interface.
#endif
#ifdef unix
  \command{man system} and \command{man sh} for how this is implemented
  on the OS in use.
#endif

  \code{\link{.Platform}} for platform-specific variables.

  \code{\link{pipe}} to set up a pipe connection.
}
#ifdef unix
\examples{
# list all files in the current directory using the -F flag
\dontrun{system("ls -F")}

# t1 is a character vector, each element giving a line of output from who
# (if the platform has who)
t1 <- try(system("who", intern = TRUE))

try(system("ls fizzlipuzzli", intern = TRUE, ignore.stderr = TRUE))
# zero-length result since file does not exist, and will give warning.
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
