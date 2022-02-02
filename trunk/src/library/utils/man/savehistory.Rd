% File src/library/utils/man/savehistory.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

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
    the history.  When supplied, only \emph{unique} matching lines are shown.}
  \item{\dots}{Arguments to be passed to \code{\link{grep}} when doing
    the matching.}
  \item{stamp}{A value or vector of values to be written into the history.}
  \item{prefix}{A prefix to apply to each line.}
  \item{suffix}{A suffix to apply to each line.}
  \item{quiet}{If \code{TRUE}, suppress printing timestamp to the console.}
}
\details{
  There are several history mechanisms available for the different \R
  consoles, which work in similar but not identical ways.  Notably,
  there are different implementations for Unix and Windows.

  \describe{
    \item{Windows:}{
  The functions described here work in \command{Rgui} and interactive
  \command{Rterm} but not in batch use of \command{Rterm} nor in
  embedded/DCOM versions.
    }
    \item{Unix-alikes:}{
  The functions described here work under the
  \command{readline} command-line interface but may not otherwise (for
  example, in batch use or in an embedded application).  Note that \R
  can be built without \command{readline}.

  \command{R.app}, the console on macOS, has a separate and largely
  incompatible history mechanism, which by default uses a file
  \file{.Rapp.history} and saves up to 250 entries.  These functions are
  not currently implemented there.
    }
  }
  The (\command{readline} on Unix-alikes) history mechanism
  is controlled by two environment variables: \env{R_HISTSIZE} controls
  the number of lines that are saved (default 512), and \env{R_HISTFILE}
  (default \file{.Rhistory}) sets the filename used for the
  loading/saving of history if requested at the beginning/end of a
  session (but not the default for
  \code{loadhistory}/\code{savehistory}).  There is no limit on the
  number of lines of history retained during a session, so setting
  \env{R_HISTSIZE} to a large value has no penalty unless a large file
  is actually generated.

  These environment variables are read at the time of saving, so can be
  altered within a session by the use of \code{\link{Sys.setenv}}.

  On Unix-alikes:
  Note that \command{readline} history library saves files with permission
  \code{0600}, that is with read/write permission for the user and not
  even read permission for any other account.

  The \code{timestamp} function writes a timestamp (or other message)
  into the history and echos it to the console.  On platforms that do not
  support a history mechanism only the console message is printed.
}
\note{
  If you want to save the history at the end of (almost) every
  interactive session (even those in which you do not save the
  workspace), you can put a call to \code{savehistory()} in
  \code{\link{.Last}}.  See the examples.
}
\examples{\dontrun{
## Save the history in the home directory: note that it is not
## (by default) read from there but from the current directory
.Last <- function()
    if(interactive()) try(savehistory("~/.Rhistory"))
}}
\keyword{utilities}
