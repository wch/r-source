% File src/library/utils/man/windows/INSTALL.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{INSTALL}
\Rdversion{1.1}
\alias{INSTALL}
\title{Install Add-on Packages from Sources}
\description{
  Utility for installing add-on source packages.
}
\usage{
\special{R CMD INSTALL [options] [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{a space-separated list with the path names of the sources of the
    packages to be installed.}
  \item{lib}{the path name of the \R library tree to install to.  Also
    accepted in the form \samp{--library=lib}.}
  \item{options}{a space-separated list of options through which in
    particular the process for building the help files can be controlled.
    Most ptions should only be given once, and paths including spaces should
    be quoted.
    Use \code{R CMD INSTALL --help} for the full current list of options.}
}
\details{
  This will stop at the first error, so if you want all the \code{pkgs}
  to be tried, call this via a shell loop.

  If used as \code{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To install into the library tree \code{lib}, use \code{R CMD INSTALL
  -l lib pkgs}.  This prepends \code{lib} to the library path for
  duration of the install, so required packages in the installation
  directory will be found (and used in preference to those in other
  libraries).

  Windows Perl and the files for installing from source packages need to
  be installed.  It is possible that environment variable \env{TMPDIR}
  will need to be set (although \code{R CMD} tries to set it suitably
  otherwise): use forward slashes and no spaces for the path to a
  writeable directory.
  
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names of directories.  \code{pkgs} may also contain
  names of package/bundle archive files of the form
  \file{pkg_version.tar.gz} as obtained from CRAN: these are then
  extracted to a temporary directory

  The package sources can be cleaned up prior to installation by
  \option{--preclean} or after by \option{--clean}: cleaning is
  essential if the sources are to be used with more than one
  platform.
  
  If \option{--no-docs} is given, no help files are built.  Options
  \option{--no-text}, \option{--no-html}, and \option{--no-latex} suppress
  creating the text, HTML, and LaTeX versions, respectively.  The
  default is to build help files in all three versions.  Similarly,
  \option{--no-example} suppresses extraction of the \R code for the
  examples from the help files.

  If the attempt to install the package fails, leftovers are removed.
  If the package was already installed, the old version is restored.
  This happens either if a command encounters an error or if the
  install is interrupted from the keyboard: after cleaning up the script
  terminates.

  By default the library directory is \sQuote{locked} by creating a
  directory \file{00LOCK} within it.  This has two purposes: it
  prevents any other process installing into that library concurrently,
  and is used to store any previous version of the package/bundle to
  restore on error.  A finer-grained locking is provided by the option
  \option{--pkglock} which creates a separate lock for each
  package/bundle: this allows enough freedom for careful parallel
  installation as done by \code{\link{install.packages}(Ncpus =
  \var{n})} with \code{\var{n} > 1}.  Finally locking (and restoration
  on error) can be suppressed by \option{--no-lock} or \option{--unsafe}
  (two names for the same option).
  
  On Windows it can be helpful to store text, latex and example code
  files and data files in zip archives since many small files can take
  up an inordinate amount of disc space.  This is controlled by the
  options \option{--use-zip-help} and \option{--use-zip-data}, and
  \option{--use-zip} which implies both.  CRAN binaries are produced by
  \option{--auto-zip} which chooses to zip help if there are more than
  20 help files, and data if the sources are more than 100Kb and there
  are no files with names differing only by extension.

  Option \option{--build} can be used to zip up the installed package or
  bundle for distribution: it implies \option{--auto-zip}.
  
  Use \code{R CMD INSTALL --help} for concise usage information,
  including all the options.
}

\note{
  Some parts of the operation of \code{INSTALL} depend on the \R
  temporary directory (see \code{\link{tempdir}}, usually under
  \file{/tmp}) having both write and execution access to the account
  running \R.  This is usually the case, but if \file{/tmp} has been
  mounted as \code{noexec}, environment variable \env{TMPDIR} may need
  to be set to a directory from which execution is allowed.
}

\section{Packages using the methods package}{
  Packages that require the methods package and make use functions such
  as \code{\link{setMethod}} or \code{\link{setClass}}, should be
  installed using lazy-loading (or, deprecated, by creating a binary
  image): use the field \code{LazyLoad} in the \file{DESCRIPTION} file
  to ensure this.
}

\seealso{
  \code{\link{REMOVE}} and \code{\link{library}} for information on
  using several library trees;
  \code{\link{update.packages}} for automatic update of packages using
  the internet (or other \R level installation of packages, such as by
  \code{install.packages}).

  The section on \dQuote{Add-on packages} in \dQuote{R Installation and
    Administration} and the chapter on \dQuote{Creating R packages} in
  \dQuote{Writing \R Extensions} (see the Help menu in \code{RGui},
  \code{\link{RShowDoc}} and the \file{doc/manual} subdirectory of the
  \R source tree).
}
\keyword{utilities}
