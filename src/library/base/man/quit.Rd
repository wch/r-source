\name{quit}
\title{Terminate an R Session}
\usage{
quit(save="ask")
   q(save="ask")
.Last <- function(x) { \dots\dots }
}
\alias{quit}
\alias{q}
\alias{.Last}
\arguments{
\item{save}{a character string indicating whether the
  environment (workspace) should be saved.}
}
\description{
The function \code{quit} or its alias \code{q} terminate the current \R
session.
\code{save} must be one of \code{"no"}, \code{"yes"},
or \code{"ask"}.
In the first case the workspace is not saved, in the second
it is saved and in the third the user is prompted and can also decide
\emph{not} to quit.

Immediately \emph{before} terminating, the function \code{.Last()} is
executed if it exists.
}
\seealso{\code{\link{.First}} for setting things on startup.
}
\examples{
.Last <- function() {
  cat("Now printing  PostScript graphic:\n")
  system("lpr Rplots.ps")
  cat("bye bye...\n")
}
## Not really:  quit("yes")
}
\keyword{environment}
