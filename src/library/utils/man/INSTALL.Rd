% File src/library/utils/man/INSTALL.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{INSTALL}
\alias{INSTALL}
\title{Install Add-on Packages}
\description{Utility for installing add-on packages.}
\usage{
\special{R CMD INSTALL [options] [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{a space-separated list with the path names of the packages to be
    installed.}
  \item{lib}{the path name of the \R library tree to install to.  Also
    accepted in the form \samp{--library=lib}.}
  \item{options}{a space-separated list of options through which in
    particular the process for building the help files can be controlled.
    Most options should only be given once, and paths including spaces should
    be quoted.  Use \command{R CMD INSTALL --help} for the full current
    list of options.}
}
\details{
  This will stop at the first error, so if you want all the \code{pkgs}
  to be tried, call this via a shell loop.

  If used as \command{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To install into the library tree \code{\var{lib}}, use
  \command{R CMD INSTALL -l \var{lib} \var{pkgs}}.
  This prepends \code{lib} to the library path for
  duration of the install, so required packages in the installation
  directory will be found (and used in preference to those in other
  libraries).
#ifdef windows

  It is possible that environment variable \env{TMPDIR}
  will need to be set (although \command{R CMD} tries to set it suitably
  otherwise): use forward slashes and no spaces for the path to a
  writable directory.
#endif
  
  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names of directories.  \code{pkgs} may also contain
  names of package archive files: these are then extracted to a
  temporary directory.  These are tarballs containing a single
  directory, optionally compressed by \command{gzip}, \command{bzip2},
  \command{xz} or \command{compress}.
#ifdef unix
  Finally, binary package archive files (as created by
  \command{R CMD INSTALL --binary}) can be supplied.
#endif
#ifdef windows
  Finally, zipped binary packages (as created by
  \command{R CMD INSTALL --binary}) can be supplied.
#endif

  Tarballs are by default unpackaged by the internal \code{\link{tar}}
  function: if needed an external \command{tar} command can be specified
  by the environment variable \env{R_INSTALL_TAR}: please ensure that it
  can handle the type of compression used on the tarball.

  The package sources can be cleaned up prior to installation by
  \option{--preclean} or after by \option{--clean}: cleaning is
  essential if the sources are to be used with more than one
  architecture or platform.
#ifdef unix  
  
  Some package sources contain a \file{configure} script that can be
  passed arguments or variables via the option \option{--configure-args}
  and \option{--configure-vars}, respectively, if necessary.  The latter
  is useful in particular if libraries or header files needed for the
  package are in non-system directories.  In this case, one can use the
  configure variables \code{LIBS} and \code{CPPFLAGS} to specify these
  locations (and set these via \option{--configure-vars}), see section
  \dQuote{Configuration variables} in \dQuote{R Installation and
  Administration} for more information.  (If these are used more than
  once on the command line they are concatenated.)  The configure
  mechanism can be bypassed using the option \option{--no-configure}.
#endif

  If the attempt to install the package fails, leftovers are removed.
  If the package was already installed, the old version is restored.
  This happens either if a command encounters an error or if the
  install is interrupted from the keyboard: after cleaning up the script
  terminates.

  By default the library directory is \sQuote{locked} by creating a
  directory \file{00LOCK} within it.  This has two purposes: it
  prevents any other process installing into that library concurrently,
  and is used to store any previous version of the package to
  restore on error.  A finer-grained locking is provided by the option
  \option{--pkglock} which creates a separate lock for each
  package: this allows enough freedom for careful parallel
  installation as done by \code{\link{install.packages}(Ncpus =
  \var{n})} with \code{\var{n} > 1}.  Finally locking (and restoration
  on error) can be suppressed by \option{--no-lock} or \option{--unsafe}
  (two names for the same option).
#ifdef windows
  
  Windows-only option \option{--build} can be used to zip up the
  installed package for distribution: it implies
  \option{--auto-zip} which selects if data file are zipped up to save
  disc space.
#endif
#ifdef unix
  
  Some platforms (notably Mac OS X) support sub-architectures in which
  binaries for different CPUs are installed within the same library
  tree. For such installations, the default behaviour is to try to build
  packages for all installed sub-architectures unless the package has a
  configure script or a \file{src/Makefile}, when only the
  sub-architecture running \command{R CMD INSTALL} is used.  To use only
  that sub-architecture, use \option{--no-multiarch}.  To install just
  the compiled code for another sub-architecture, use
  \option{--libs-only}.
#endif
  
  Use \command{R CMD INSTALL --help} for concise usage information,
  including all the available options
}

#ifdef unix
\note{
  Some parts of the operation of \code{INSTALL} depend on the \R
  temporary directory (see \code{\link{tempdir}}, usually under
  \file{/tmp}) having both write and execution access to the account
  running \R.  This is usually the case, but if \file{/tmp} has been
  mounted as \code{noexec}, environment variable \env{TMPDIR} may need
  to be set to a directory from which execution is allowed.
}
#endif

\section{Packages using the methods package}{
  Packages that require the methods package and make use functions such
  as \code{\link{setMethod}} or \code{\link{setClass}}, should be
  installed using lazy-loading: use the field \code{LazyLoad} in the
  \file{DESCRIPTION} file to ensure this.
}

\seealso{
  \code{\link{REMOVE}} and \code{\link{library}} for information on
  using several library trees;
  \code{\link{update.packages}} for automatic update of packages using
  the internet (or other \R level installation of packages, such as by
  \code{install.packages}).

  The section on \dQuote{Add-on packages} in \dQuote{R Installation and
    Administration} and the chapter on \dQuote{Creating R packages} in
  \dQuote{Writing \R Extensions}
#ifdef windows
  (on the Help menu in \code{RGui},
#endif
  \code{\link{RShowDoc}} and the \file{doc/manual} subdirectory of the
  \R source tree).
}
\keyword{utilities}
