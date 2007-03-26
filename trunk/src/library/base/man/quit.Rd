\name{quit}
\alias{quit}
\alias{q}
\alias{.Last}
\alias{.Last.sys}
\title{Terminate an R Session}
\usage{
quit(save = "default", status = 0, runLast = TRUE)
   q(save = "default", status = 0, runLast = TRUE)
.Last <- function(x) { \dots\dots }
}
\arguments{
    \item{save}{a character string indicating whether the environment
        (workspace) should be saved, one of \code{"no"}, \code{"yes"},
        \code{"ask"} or \code{"default"}.}
    \item{status}{the (numerical) error status to be returned to the
        operating system, where relevant. Conventionally \code{0}
        indicates successful completion.}
    \item{runLast}{should \code{.Last()} be executed?}
}
\description{
    The function \code{quit} or its alias \code{q} terminate the current
    \R session.
}
\details{
    \code{save} must be one of \code{"no"}, \code{"yes"},
    \code{"ask"} or \code{"default"}.  In the first case the workspace
    is not saved, in the second it is saved and in the third the user is
    prompted and can also decide \emph{not} to quit.  The default is to
    ask in interactive use but may be overridden by command-line
    arguments (which must be supplied in non-interactive use).

    Immediately \emph{before} terminating, the function \code{.Last()}
    is executed if it exists and \code{runLast} is true. If in
    interactive use there are errors in
    the \code{.Last} function, control will be returned to the command
    prompt, so do test the function thoroughly.  There is a system
    analogue, \code{.Last.sys()}. which is run after \code{.Last()}
    if \code{runLast} is true.

    Some error statuses are used by \R itself.  The default error
    handler for non-interactive effectively calls \code{q("no", 1,
      FALSE)} and returns error code 1.  Error status 2 is used for \R
    \sQuote{suicide}, that is a catastrophic failure, and other small
    numbers are used by specific ports for initialization failures.  It
    is recommended that users choose statuses of 10 or more.

    Valid values of \code{status} are system-dependent, but \code{0:255}
    are normally valid.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
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
