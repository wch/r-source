\name{quit}
\alias{quit}
\alias{q}
\alias{.Last}
\title{Terminate an R Session}
\usage{
quit(save="default")
   q(save="default")
.Last <- function(x) { \dots\dots }
}
\arguments{
    \item{save}{a character string indicating whether the environment
	(workspace) should be saved.}
}
\description{
    The function \code{quit} or its alias \code{q} terminate the current
    \R session.  \code{save} must be one of \code{"no"}, \code{"yes"},
    \code{"ask"} or \code{"default"}.  In the first case the workspace
    is not saved, in the second it is saved and in the third the user is
    prompted and can also decide \emph{not} to quit.  The default is to
    ask in interactive use and not to save in non-interactive use, but
    may be overridden by command-line arguments.

    Immediately \emph{before} terminating, the function \code{.Last()}
    is executed if it exists. If in interactive use there are errors in
    the \code{.Last} function, control will be returned to the command
    prompt, so do test the function thoroughly.
}
\seealso{\code{\link{.First}} for setting things on startup.
}
\examples{
\dontrun{## Unix-flavour example
.Last <- function() {
  cat("Now sending PostScript graphics to the printer:\n")
  system("lpr Rplots.ps")
  cat("bye bye...\n")
}
quit("yes")}
}
\keyword{environment}
