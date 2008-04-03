% File src/library/base/man/Startup.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{Startup}
\alias{Startup}
\alias{Rprofile}
\alias{.Rprofile}
\alias{Rprofile.site}
\alias{Renviron}
\alias{Renviron.site}
\alias{.Renviron}
\alias{.First}
\alias{.First.sys}
\alias{.OptRequireMethods}
\concept{environment variable}

\title{Initialization at Start of an R Session}
\usage{
.First <- function() { \dots\dots }

\special{.Rprofile <startup file>}
}
\description{
  In \R, the startup mechanism is as follows.

  Unless \option{--no-environ} was given on the command line, \R
  searches for site and user files to process for setting environment
  variables.  The name of the site file is the one pointed to by the
  environment variable \env{R\_ENVIRON}; if this is unset or empty,
  \file{\var{\link[base:Rhome]{R\_HOME}}/etc/Renviron.site} is used (if it exists, which it
  does not in a \sQuote{factory-fresh} installation).
  The user files searched for are \file{.Renviron} in the current or in
  the user's home directory (in that order).
#ifdef windows
  (See \sQuote{See also} for how the home directory is found on Windows.)
#endif
  See \sQuote{Details} for how the files are read.

  Then \R searches for the site-wide startup profile unless the command
  line option \option{--no-site-file} was given.  The name of this file
  is taken from the value of the \env{R\_PROFILE} environment variable.
  If this variable is unset, the default is
  \file{\var{\link[base:Rhome]{R\_HOME}}/etc/Rprofile.site}, which is used if it exists
#ifdef unix
  (which it does not in a \sQuote{factory-fresh} installation).
#endif
#ifdef windows
  (it contains settings from the installer in a \sQuote{factory-fresh}
  installation).
#endif
  This code is sourced into the \pkg{base} package.  Users need to be
  careful not to unintentionally overwrite objects in \pkg{base}, and it
  is normally advisable to use \code{\link{local}} if code needs to be
  executed: see the examples.

  Then, unless \option{--no-init-file} was given, \R searches for a file
  called \file{.Rprofile} in the current directory or in the user's
  home directory (in that order) and sources it into the user
  workspace.

  Note that when the site and user profile files are sourced only the
  \pkg{base} package is loaded, so objects in other packages need to be
  referred to by e.g. \code{utils::dump.frames} or after explicitly
  loading the package concerned.

  It then loads a saved image of the user workspace from \file{.RData}
  if there is one (unless \option{--no-restore-data} or
  \option{--no-restore} was specified on the command line).

  Next, if a function \code{.First} is found on the search path,
  it is executed as \code{.First()}.  Finally, function
  \code{.First.sys()} in the \pkg{base} package is run. This calls
  \code{\link{require}} to attach the default packages specified by
  \code{\link{options}("defaultPackages")}.  If the \pkg{methods}
  package is included, this will have been attached earlier (by function
  \code{.OptRequireMethods()}) so that name space initializations such
  as those from the user workspace will proceed correctly.

  A function \code{.First} (and \code{\link{.Last}}) can be defined in
  appropriate \file{.Rprofile} or \file{Rprofile.site} files or have
  been saved in \file{.RData}.  If you want a different set of packages
  than the default ones when you start, insert a call to
  \code{\link{options}} in the \file{.Rprofile} or \file{Rprofile.site}
  file.  For example, \code{options(defaultPackages = character())} will
  attach no extra packages on startup (only the \code{base} package) (or
  set \code{R_DEFAULT_PACKAGES=NULL} as an environment variable before
  running \R).  Using \code{options(defaultPackages = "")} or
  \code{R_DEFAULT_PACKAGES=""} enforces the R \emph{system} default.

  On front-ends which support it, the commands history is read from the
  file specified by the environment variable \env{R\_HISTFILE} (default
  \file{.Rhistory}) unless \option{--no-restore-history} was specified
  (or \option{--no-restore}).

  The command-line flag \option{--vanilla} implies
  \option{--no-site-file}, \option{--no-init-file},
  \option{--no-restore} and \option{--no-environ}.
#ifdef windows
  Under Windows, it also implies \option{--no-Rconsole}, which
  prevents loading the \file{\link{Rconsole}} file.
#endif
}
\details{
  Note that there are two sorts of files used in startup:
  \emph{environment files} which contain lists of environment variables
  to be set, and \emph{profile files} which contain \R code.

  Lines in a site or user environment file should be either comment
  lines starting with \code{#}, or lines of the form
  \code{\var{name}=\var{value}}. The latter sets the environmental
  variable \code{\var{name}} to \code{\var{value}}, overriding an
  existing value.  If \code{\var{value}} contains an expression of the
  form \code{${foo-bar}}, the value is that of the environmental
  variable \code{foo} if that exists and is set to a non-empty value,
  otherwise \code{bar}.  (If it is of the form \code{${foo}}, the
  default is \code{""}.)  This construction can be nested, so \code{bar}
  can be of the same form (as in \code{${foo-${bar-blah}}}).  Note that
  the braces are essential: \code{$HOME} will not be interpreted.

  Leading and trailing white space in \code{\var{value}} are stripped.
  \code{\var{value}} is then processed in a similar way to a Unix shell:
  in particular the outermost level of (single or double) quotes is
  stripped, and backslashes are removed except inside quotes.
}
\note{
#ifdef unix
  The file \file{\var{\link[base:Rhome]{R\_HOME}}/etc/Renviron} is always read very early in
  the start-up processing.  It contains environment variables set by \R
  in the configure process.  Values in that file can be overridden in
  site or user environment files: do not change
  \file{\var{\link[base:Rhome]{R\_HOME}}/etc/Renviron} itself.  Note that this is distinct from
  \file{\var{\link[base:Rhome]{R\_HOME}}/etc/Renviron.site}.
#endif
#ifdef windows
  Unix versions of \R have a file \file{\var{\link[base:Rhome]{R\_HOME}}/etc/Renviron}
  which is read very early in the start-up processing.  It contains
  environment variables set by \R in the configure process, and is not
  used on \R for Windows.
#endif
}
\seealso{
#ifdef windows
  For the definition of the home directory see the rw-FAQ.  It can be
  found from a running \R by \code{Sys.getenv("R_USER")}.

#endif
  \code{\link{.Last}} for final actions at the close of an \R session.
  \code{\link{commandArgs}} for accessing the command line arguments.

  \emph{An Introduction to R} for more command-line options: those
  affecting memory management are covered in the help file for
  \link{Memory}.

  For profiling code, see \code{\link{Rprof}}.
}
\examples{
\dontrun{
## Example ~/.Renviron on Unix
R_LIBS=~/R/library
PAGER=/usr/local/bin/less

## Example .Renviron on Windows
R_LIBS=C:/R/library
MY_TCLTK="c:/Program Files/Tcl/bin"

## Example of setting R_DEFAULT_PACKAGES (from R CMD check)
R_DEFAULT_PACKAGES='utils,grDevices,graphics,stats'
# this loads the packages in the order given, so they appear on
# the search path in reverse order.

## Example of .Rprofile
options(width=65, digits=5)
options(show.signif.stars=FALSE)
setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::ps.options(horizontal=FALSE))
set.seed(1234)
.First <- function() cat("\n   Welcome to R!\n\n")
.Last <- function()  cat("\n   Goodbye!\n\n")

## Example of Rprofile.site
local({
  # add MASS to the default packages, set a CRAN mirror
  old <- getOption("defaultPackages"); r <- getOption("repos")
  r["CRAN"] <- "http://my.local.cran"
  options(defaultPackages = c(old, "MASS"), repos = r)
})

## if .Renviron contains
FOOBAR="coo\bar"doh\\ex"abc\"def'"

## then we get
> cat(Sys.getenv("FOOBAR"), "\n")
coo\bardoh\exabc"def'
}}
\keyword{environment}
