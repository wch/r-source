\name{Startup}
\alias{Startup}
\alias{Rprofile}
\alias{.Rprofile}
\alias{Rprofile.site}
\alias{Renviron.site}
\alias{.Renviron}
\alias{.First}
\alias{.First.sys}
\concept{environment variable}

\title{Initialization at Start of an R Session}
\usage{
.First <- function() { \dots\dots }

\special{.Rprofile <startup file>}
}
\description{
  In \R, the startup mechanism is as follows.

  Unless \option{--no-environ} was given on the command line, \R
  searches for user and site files to process for setting environment
  variables.  The name of the site file is the one pointed to by the
  environment variable \env{R\_ENVIRON}; if this is unset or empty,
  \file{\$R\_HOME/etc/Renviron.site} is used (if it exists, which it
  does not in a \dQuote{factory-fresh} installation except on Windows).
  The user files searched for are \file{.Renviron} in the current or in
  the user's home directory (in that order).
  See \bold{Details} for how the files are read.

  Then \R searches for the site-wide startup profile unless the command
  line option \option{--no-site-file} was given.  The name of this file
  is taken from the value of the \env{R\_PROFILE} environment variable.
  If this variable is unset, the default is
  \file{\$R\_HOME/etc/Rprofile.site}, which is used if it exists
  (which it does not in a \dQuote{factory-fresh} installation).
  This code is loaded into package \pkg{base}.  Users need to be
  careful not to unintentionally overwrite objects in base, and it
  is normally advisable to use \code{\link{local}} if code needs to be
  executed: see the examples.

  Then, unless \option{--no-init-file} was given, \R searches for a file
  called \file{.Rprofile} in the current directory or in the user's
  home directory (in that order) and sources it into the user
  workspace.

  It then loads a saved image of the user workspace from \file{.RData}
  if there is one (unless \option{--no-restore-data} was specified, or
  \option{--no-restore}, on the command line).

  Next, if a function \code{.First} is found on the search path,
  it is executed as \code{.First()}.
  Finally, function \code{.First.sys()} in the \pkg{base} package is run.
  This calls \code{\link{require}} to attach the default packages
  specified by \code{\link{options}("defaultPackages")}.

  A function \code{.First} (and \code{\link{.Last}}) can be defined in
  appropriate \file{.Rprofile} or \file{Rprofile.site} files or have
  been saved in \file{.RData}.  If you want a different set of packages
  than the default ones when you start, insert a call to
  \code{\link{options}} in the \file{.Rprofile} or \file{Rprofile.site}
  file.  For example, \code{options(defaultPackages = character())} will
  attach no extra packages on startup.  Alternatively, set
  \code{R_DEFAULT_PACKAGES=NULL} as an environment variable before
  running \R.  Using \code{options(defaultPackages = "")} or
  \code{R_DEFAULT_PACKAGES=""} enforces the R \emph{system} default.

  The commands history is read from the file specified by the
  environment variable \env{R\_HISTFILE} (default \file{.Rhistory})
  unless \option{--no-restore-history} was specified (or
  \option{--no-restore}).

  The command-line flag \option{--vanilla} implies
  \option{--no-site-file}, \option{--no-init-file},
  \option{--no-restore} and \option{--no-environ}.
}
\details{
  Note that there are two sorts of files used in startup:
  \emph{environment files} which contain lists of environment variables
  to be set, and \emph{profile files} which contain \R code.

  Lines in a site or user environment file should be either comment
  lines starting with \code{#}, or lines of the form
  \code{name=value}. The latter sets the environmental variable
  \code{name} to \code{value}, overriding an existing value.  If
  \code{value} contains an expression of the form \code{${foo-bar}}, the
  value is that of the environmental variable \code{foo} if that exists
  and is set to a non-empty value, otherwise \code{bar}.  (If it is of
  the form \code{${foo}}, the default is \code{""}.)  This construction
  can be nested, so \code{bar} can be of the same form (as in
  \code{${foo-${bar-blah}}}).  Note that the braces are essential:
  \code{$HOME} will not be interpreted.

  Leading and trailing white space in \code{value} are stripped.
  \code{value} is processed in a similar way to a Unix shell.  In particular
  the outermost level of (single or double) quotes is stripped, and
  backslashes are removed except inside quotes.
}
#ifdef unix
\note{
  The file \file{\$R\_HOME/etc/Renviron} is always read very early in
  the start-up processing.  It contains environment variables set by \R
  in the configure process.  Values in that file can be overriden in
  site or user environment files: do not change
  \file{\$R\_HOME/etc/Renviron} itself.  Note that this is distinct from
  \file{\$R\_HOME/etc/Renviron.site}.
}
#endif
\section{Historical notes}{
  Prior to \R version 1.4.0, the environment files searched were
  \file{.Renviron} in the current directory, the file pointed to by
  \env{R\_ENVIRON} if set, and \file{.Renviron} in the user's home
  directory.  What is now \file{Rprofile.site} was called \file{Rprofile}.

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
  \code{\link{commandArgs}} for accessing the command line arguments.

  \emph{An Introduction to R} for more command-line options: those
  affecting memory management are covered in the help file for
  \link{Memory}.

  For profiling code, see \code{\link{Rprof}}.
}
\examples{
\dontrun{
# Example ~/.Renviron on Unix
R_LIBS=~/R/library
PAGER=/usr/local/bin/less

# Example .Renviron on Windows
R_LIBS=C:/R/library
MY_TCLTK=yes
TCL_LIBRARY=c:/packages/Tcl/lib/tcl8.4

# Example of .Rprofile
options(width=65, digits=5)
options(show.signif.stars=FALSE)
ps.options(horizontal=FALSE)
set.seed(1234)
.First <- function() cat("\n   Welcome to R!\n\n")
.Last <- function()  cat("\n   Goodbye!\n\n")

# Example of Rprofile.site
local({
  old <- getOption("defaultPackages")
  options(defaultPackages = c(old, "MASS"))
})

## if .Renviron contains
FOOBAR="coo\bar"doh\\ex"abc\"def'"

## then we get
> cat(Sys.getenv("FOOBAR"), "\n")
coo\bardoh\exabc"def'
}}
\keyword{environment}
