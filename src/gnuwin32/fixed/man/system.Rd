\name{system}
\title{Invoke a System Command}
\usage{
system(command, intern=FALSE, wait=TRUE, input,
       show.output.on.console=FALSE
       invisible=FALSE, minimized=FALSE)
.Platform
}
\alias{system}
\alias{.Platform}
\alias{unix}
\arguments{
    \item{command}{the system command to be invoked, as a string.}
    \item{intern}{a logical, indicates whether to make the output of the
	command an \R object.}
    \item{wait}{should the \R interpreter wait for the command to finish?
	The default is to wait, and the interpreter will always wait if
	\code{intern = TRUE}.}
    \item{input}{if a character vector is supplied, this is copied one
	string per line to a temporary file, and the standard input of
	\code{command} is redirected to the file.}
    \item{show.output.on.console}{a logical, indicates whether to capture 
          the output of the command and show it on the \R console (not used
	  by \code{Rterm}, which captures the output unless \code{wait}
	  is false).}
    \item{invisible}{a logical, indicates whether the command window
	  should be visible on the screen.}
    \item{minimized}{a logical, indicates whether the command window
	  should be initially displayed as iconic.}      
}
\description{
    \code{system} invokes the system command specified by \code{command}.
    
    \code{.Platform} is a list with functions and variables as
    components.
}
\value{
    If \code{intern=TRUE}, a character vector giving the output of the
    command, one line per character string. If the command could not be
    run or gives an error a \R error is generated.

    If \code{intern=FALSE}, the return value is a error code, given the
    invisible attribute (so needs to be printed explictly). If the
    command could not be run for any reason, the value is \code{-1} and
    an \R warning is generated. Otherwise if \code{wait=FALSE} the value
    is the error code returned by the command, and if \code{wait=TRUE}
    it is the zero (the conventional success value),

    If \code{intern=FALSE} and \code{show.output.on.console=TRUE} 
    the text output from a command that is a console application will 
    appear in the \R console (\code{Rgui}) or the window running \R 
    (\code{Rterm}).
}
\details{
    The command is run directly as a Windows command by the Windows API
    call \code{CreateProcess}: extensions of \code{.exe}, \code{.com},
    \code{.cmd} and \code{.bat} are tried in turn if none
    is supplied. (To use DOS internal commands use
    \code{command.com /c cmd}.) The search path for \code{command}
    may be system-dependent: it will include the \R \code{bin}
    directory, the working directory and the Windows system directories
    before \code{PATH}.

    Precisely what is seen by the user depends on whether \code{Rgui} or
    \code{Rterm} is being used.  For \code{Rgui} a new console will
    always be used, so a commands window will appear for the duration of
    console applications unless \code{invisible} is true. For
    \code{Rterm} a separate commands window
    will appear for console applications only if \code{wait=FALSE}.

    \code{unix} is a \emph{deprecated} alternative, available for
    backwards compatibility.
}
\section{WARNING}{
    The command cannot be interrupted by the \R process.

    Do not run console applications that require user
    input from \code{Rgui} setting \code{intern=TRUE} and/or
    \code{show.output.on.console=TRUE}. They will not work,
    may hang and then will probably hang \code{Rgui} too.
}

\author{Guido Masarotto and Brian Ripley}

\seealso{\code{\link{shell}} for a less raw interface.}

\examples{
# launch an editor, wait for it to quit
\dontrun{system("notepad myfile.txt")}
# launch a Windows 9x process monitor (from Win9x KernelToys)
\dontrun{system("wintop", wait=FALSE)}
# launch your favourite (!) shell: 
\dontrun{system("command.com")}
}
\keyword{interface}
\keyword{file}
\keyword{utilities}
