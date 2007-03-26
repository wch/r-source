\name{savehistory}
\alias{loadhistory}
\alias{savehistory}
\alias{history}
\alias{timestamp}
\title{Load or Save or Display the Commands History}
\description{
  Load or save or display the commands history.
}
\usage{
loadhistory(file = ".Rhistory")
savehistory(file = ".Rhistory")

history(max.show = 25, reverse = FALSE, pattern, \dots)

timestamp(stamp = date(),
          prefix = "##------ ", suffix = " ------##",
          quiet = FALSE)
}
\arguments{
  \item{file}{The name of the file in which to save the history, or
    from which to load it. The path is relative to the current
    working directory.}
  \item{max.show}{The maximum number of lines to show. \code{Inf} will
    give all of the currently available history.}
  \item{reverse}{logical. If true, the lines are shown in reverse
    order. Note: this is not useful when there are continuation lines.}
  \item{pattern}{A character string to be matched against the lines of
    the history}
  \item{\dots}{Arguments to be passed to \code{\link{grep}} when doing
    the matching.}
  \item{stamp}{A value or vector of values to be written into the history.}
  \item{prefix}{A prefix to apply to each line.}
  \item{suffix}{A suffix to apply to each line.}
  \item{quiet}{If \code{TRUE}, suppress printing timestamp to the console.}
}
\details{
  There are several history mechanisms available for the different \R
  consoles, which work in similar but not identical ways.  Other uses of
  \R, in particular embedded uses, may have no history.
#ifdef windows
  This works in Rgui and interactive Rterm but not in batch use of Rterm
  nor in embedded/DCOM versions.

  The history mechanism is controlled by two environment
#endif
#ifdef unix
  This works under the \code{readline} and GNOME and MacOS X consoles,
  but not otherwise (for example, in batch use or in an embedded application).

  The \code{readline} history mechanism is controlled by two environment
#endif
  variables: \code{R_HISTSIZE} controls the number of lines that are
  saved (default 512), and \code{R_HISTFILE} sets the filename used for
  the loading/saving of history if requested at the beginning/end of a
  session (but not the default for these functions).  There is no limit
  on the number of lines of history retained during a session, so
  setting \code{R_HISTSIZE} to a large value has no penalty unless a
  large file is actually generated.

  These variables are read at the time of saving, so can be altered
  within a session by the use of \code{\link{Sys.setenv}}.

  \code{history} shows only unique matching lines if \code{pattern} is
  supplied.
  
  The \code{timestamp} function writes a timestamp (or other message)
  into the history and echos it to the console.  On platforms that do not 
  support a history mechanism (where the mechanism does not yet support
  timestamps) only the console message is printed.
}
\note{
  If you want to save the history (almost) every session, you can put a
  call to \code{savehistory()} in \code{\link{.Last}}.  See the examples.
}
\examples{\dontrun{
.Last <- function()
    if(interactive()) try(savehistory("~/.Rhistory"))
}}
\keyword{utilities}
