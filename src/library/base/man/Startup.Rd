\name{Startup}
\title{Initialization at Start of an R Session}
\usage{
.First <- function() { \dots\dots }

.Rprofile <startup file>
}
\alias{Startup}
\alias{Rprofile}
\alias{.Rprofile}
\alias{.First}
\description{
  In \R, the startup mechanism is as follows.  If a file
  \file{.Renviron}, or a file pointed to by \code{R\_ENVIRON} or
  \file{\eqn{\sim}{~}/.Renviron}, exists, it is processed to set
  environmental variables.  (This is suppressed by the flag
  \code{--no-environ}.)   The lines in these files must be comment lines
  starting with a \code{#}, or of the form \code{name=value} where
  \code{value} is subject to shell parameter expansion; in particular,
  the form \code{${foo-"bar"}} can be used for providing default values
  (this construction can be nested as well).

  Then \R searches for the site-wide startup profile unless the command
  line option \code{--no-site-file} was given.  The name of this file is
  taken from the value of the \code{R\_PROFILE} environment variable.
  If this variable is unset, the default is \file{\$R\_HOME/etc/Rprofile}.
  This code is loaded into package \code{base}.

  Then, unless \code{--no-init-file} was given, \R searches for a file
  called \file{.Rprofile} in the current directory or in the user's
  home directory (in that order) and sources it into the user
  workspace.

  It then loads a saved image of the user workspace from \file{.RData}
  if there is one (unless \code{--no-restore-data} was specified, or
  \code{--no-restore}).
  
  Finally, if a function \code{.First} exists, it is executed as
  \code{.First()}.

  The functions \code{.First} and \code{\link{.Last}} can be defined
  in the appropriate startup profiles or reside in \file{.RData}.

  The commands history is read from the file specified by the
  environment variable \code{R_HISTFILE} (default \code{.Rhistory})
  unless \code{--no-restore-history} was specified (or
  \code{--no-restore}).

  The command-line flag \code{--vanilla} implies \code{--no-init-file},
  \code{--no-restore} and \code{--no-environ}.
}
\note{
  Prior to \R version 1.2.1, \file{.Rprofile} was sourced after
  \file{.RData} was loaded, although the documented order was as here.
}
\seealso{
  \code{\link{.Last}} for final actions before termination.
}
\keyword{environment}
