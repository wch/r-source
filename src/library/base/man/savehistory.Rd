\name{savehistory}
\alias{loadhistory}
\alias{savehistory}
\alias{history}
\title{Load or Save or Display the Commands History}
\description{
  Load or save or display the commands history.
}
\usage{
loadhistory(file = ".Rhistory")
savehistory(file = ".Rhistory")
history(max.show = 25, reverse = FALSE)
}
\arguments{
  \item{file}{The name of the file in which to save the history, or
    from which to load it. The path is relative to the current
    working directory.}
  \item{max.show}{The maximum number of lines to show. \code{Inf} will
    give all of the currently available history.}
  \item{reverse}{logical. If true, the lines are shown in reverse
    order. Note: this is not useful when there are continuation lines.}
}
\details{
#ifdef windows
  This works in Rgui and interactive Rterm but not in batch use of Rterm
  nor in embedded/DCOM versions.
#endif
#ifdef unix
  This works under the \code{readline} and GNOME interfaces, but not if
  \code{readline} is not available (for example, in batch use).
#endif
}
\note{
  If you want to save the history (almost) every session, you can put a
  call to \code{savehistory()} in \code{\link{.Last}}.
}
\examples{\dontrun{
.Last <- function()
    if(interactive()) try(savehistory("~/.Rhistory"))
}}
\keyword{utilities}
