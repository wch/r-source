\name{Startup}
\alias{Startup}
\alias{Rprofile}
\alias{.Rprofile}
#ifndef mac
\alias{Rprofile.site}
#endif
#ifdef unix
\alias{Renviron.site}
#endif
\alias{.Renviron}
\alias{.First}
\title{Initialization at Start of an R Session}
\usage{
.First <- function() { \dots\dots }

.Rprofile <startup file>
}
\description{
  In \R, the startup mechanism is as follows.

#ifdef unix
  Unless \code{--no-environ} was given on the command line,
  \R searches for user and site
  files to process for setting environment variables.  The name of the
  site file is the one pointed to by the environment variable
  \code{R\_ENVIRON}; if this is unset or empty,
  \file{\$R\_HOME/etc/Renviron.site} is used (if it exists, which it
  does not in a ``factory-fresh'' installation).
  The user files searched for are \file{.Renviron} in the
  current or in the user's home directory (in that order).
  See \bold{Details} for how the files are read.

#endif
#ifdef windows
  Unless \code{--no-environ} was given on the command line,
  \R searches for a user file to process for setting environment variables.
  The search is for \file{.Renviron} in the
  current or in the user's home directory (in that order).
  See \bold{Details} for how the file is read.

#endif
#ifndef mac
  Then \R searches for the site-wide startup profile unless the command
  line option \code{--no-site-file} was given.  The name of this file is
  taken from the value of the \code{R\_PROFILE} environment variable.
  If this variable is unset, the default is
  \file{\$R\_HOME/etc/Rprofile.site}, which is used if it exists
  (which it does not in a ``factory-fresh'' installation).
  This code is loaded into package \code{base}.

  Then, unless \code{--no-init-file} was given, \R searches for a file
  called \file{.Rprofile} in the current directory or in the user's
  home directory (in that order) and sources it into the user
  workspace.
#endif
#ifdef mac
  Unless \code{--no-environ} was given, \R reads the file
  \file{.Renviron} in the \file{etc} subdirectory of the \R application
  folder.  See \bold{Details} for how the file is read.

  Then, unless \code{--no-init-file} was given, \R looks for a file
  called \file{.Rprofile} in the \file{etc} subdirectory of \R
  application folder and sources it into the user workspace.
#endif

  It then loads a saved image of the user workspace from \file{.RData}
  if there is one (unless \code{--no-restore-data} was specified, or
  \code{--no-restore}, on the command line).

  Finally, if a function \code{.First} is found on the search path,
  it is executed as \code{.First()}.  In a ``factory-fresh'' version of
  \R the \code{.First} in the system profile file
  \file{library/base/R/Rprofile} is used.  (This \code{\link{require}}s
  the \code{ctest} library.)

  A function \code{.First} (and \code{\link{.Last}}) can be defined
  in appropriate \code{.Rprofile} or \code{Rprofile.site} files
  or have been saved in \file{.RData}.

  The commands history is read from the file specified by the
  environment variable \code{R_HISTFILE} (default \code{.Rhistory})
  unless \code{--no-restore-history} was specified (or
  \code{--no-restore}).

  The command-line flag \code{--vanilla} implies \code{--no-site-file},
  \code{--no-init-file}, \code{--no-restore} and \code{--no-environ}.
}
\details{
  Note that there are two sorts of files used in startup:
  \emph{environment files} which contain lists of environment variables
  to be set, and \emph{profile files} which contain \R code.

  Lines in a site or user environment file should be either comment
  lines starting with \code{#}, or lines of the form \code{name=value}.
  The latter sets the environmental variable \code{name} to
  \code{value}, overriding an existing value.  If \code{value} is of the
  form \code{${foo-bar}}, the value is that of the environmental
  variable \code{foo} if that exists and is set to a non-empty value,
  otherwise \code{bar}.  This construction can be nested, so
  \code{bar} can be of the same form (as in \code{${foo-${bar-blah}}}).

  Leading and trailing white space in \code{value} are stripped.
%  No other interpretation of \code{value} is performed. In particular,
%  if it is enclosed in quotes, the quotes form part of the value.
  \code{value} is processed in a similar way to a Unix shell.  In particular
  quotes are stripped, and backslashes are removed except inside quotes.
}
\note{
#ifdef Unix
  The file \file{\$R\_HOME/etc/Renviron} is always read very early in
  the start-up processing.  It contains environment variables set by \R
  in the configure process.  Values in that file can be overriden in
  site or user environment files: do not change
  \file{\$R\_HOME/etc/Renviron} itself.

#endif
  Prior to \R version 1.4.0, the environment files searched were
  \file{.Renviron} in the current directory, the file pointed to by
  \code{R_ENVIRON} if set, and \file{.Renviron} in the user's home
  directory.

  Prior to \R version 1.2.1, \file{.Rprofile} was sourced after
  \file{.RData} was loaded, although the documented order was as here.

  The format for site and user environment files was changed in version
  1.2.0.  Older files are quite likely to work but may generate warnings
  on startup if they contained unnecessary \code{export} statements.

  Values in environment files were not processed prior to version
  1.4.0.
}
\seealso{
  \code{\link{.Last}} for final actions before termination.
}
\examples{
\dontrun{
# Example ~/.Renviron on Unix
R_LIBS=~/R/library
PAGER=/usr/local/bin/less

# Example .Renviron on Windows
R_LIBS=C:/R/library
TCL_LIBRARY=c:/packages/Tcl/lib/tcl8.3

# Example of .Rprofile
options(width=65, digits=5)
options(show.signif.stars=FALSE)
ps.options(horizontal=FALSE)
set.seed(1234)
.First <- function() cat("\n   Welcome to R!\n\n")
.Last <- function()  cat("\n   Goodbye!\n\n")

## if .Renviron contains
FOOBAR="coo\bar"doh\\ex"abc\"def'"

## then we get
> cat(Sys.getenv("FOOBAR"), "\n")
coo\bardoh\exabc"def'
}}
\keyword{environment}
