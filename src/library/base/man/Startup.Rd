\name{Startup}
\alias{Startup}
\alias{Rprofile}
\alias{.Rprofile}
\alias{Rprofile.site}
\alias{Renviron.site}
\alias{.Renviron}
\alias{.First}
\title{Initialization at Start of an R Session}
\usage{
.First <- function() { \dots\dots }

.Rprofile <startup file>
}
\description{
  In \R, the startup mechanism is as follows.

  Unless \code{--no-environ} was given, \R searches for user and site
  files to process for setting environment variables.  The name of the
  site file is the one pointed to by the environment variable
  \code{R\_ENVIRON}; if this is unset,
  \file{\$R\_HOME/etc/Renviron.site}
  is used.  The user files searched for are \file{.Renviron} in the
  current or in the user's home directory (in that order).
  See \bold{Details} for how the files are read.

  Then \R searches for the site-wide startup profile unless the command
  line option \code{--no-site-file} was given.  The name of this file is
  taken from the value of the \code{R\_PROFILE} environment variable.
  If this variable is unset, the default is
  \file{\$R\_HOME/etc/Rprofile.site}.
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
\details{
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
  \code{value} is processed in a similar to a Unix shell.  In particular
  quotes are stripped, and backslashes are removed except inside quotes.
}
\note{
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
\dontrun{## Some examples with a Unix flavour
# ~/.Renviron
R_LIBS=~/R/library
PAGER=/usr/local/bin/less

# .Rprofile
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
