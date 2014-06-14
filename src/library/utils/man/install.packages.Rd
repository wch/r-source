% File src/library/utils/man/install.packages.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{install.packages}
\alias{install.packages}
\title{Install Packages from Repositories or Local Files}
\description{
  Download and install packages from CRAN-like repositories or from
  local files.
}
\usage{
install.packages(pkgs, lib, repos = getOption("repos"),
                 contriburl = contrib.url(repos, type),
                 method, available = NULL, destdir = NULL,
                 dependencies = NA, type = getOption("pkgType"),
                 configure.args = getOption("configure.args"),
                 configure.vars = getOption("configure.vars"),
                 clean = FALSE, Ncpus = getOption("Ncpus", 1L),
                 verbose = getOption("verbose"),
                 libs_only = FALSE, INSTALL_opts, quiet = FALSE,
                 keep_outputs = FALSE, \dots)
}
\arguments{
  \item{pkgs}{character vector of the names of packages whose
    current versions should be downloaded from the repositories.

    If \code{repos = NULL}, a character vector of
#ifdef windows
    file paths of \file{.zip} files containing binary builds of
    packages.  (\code{http://} and \code{file://} URLs are also accepted
    and the files will be downloaded and installed from local copies.)
    Source directories or file paths or URLs of archives be
    specified with \code{type = "source"}, but some packages need
    suitable tools installed (see the \sQuote{Details} section).
#endif
#ifdef unix
    file paths.  These can be source directories or archives
    or binary package archive files (as created by \command{R CMD build
      --binary}).  (\code{http://} and \code{file://} URLs are also
    accepted and the files will be downloaded and installed from local
    copies.)  On a CRAN build of \R for OS X these can be \file{.tgz}
    files containing binary package archives.
    Tilde-expansion will be done on file paths.
#endif

    If this is missing or a zero-length character vector, a listbox of
    available packages is presented where possible in an interactive \R
    session.
  }
  \item{lib}{
    character vector giving the library directories where to
    install the packages.  Recycled as needed.  If missing, defaults to
    the first element of \code{\link{.libPaths}()}.
  }
  \item{repos}{
    character vector, the base URL(s) of the repositories
    to use, e.g., the URL of a CRAN mirror such as
    \code{"http://cran.us.r-project.org"}.  For more details on
    supported URL schemes see \code{\link{url}}.

    Can be \code{NULL} to install from local files, directories or URLs:
    this will be inferred if possible from \code{pkgs}.
  }
  \item{contriburl}{
    URL(s) of the contrib sections of the repositories.  Use this
    argument if your repository mirror is incomplete, e.g., because
    you burned only the \file{contrib} section on a CD, or only have
    binary packages.  Overrides argument \code{repos}.  Can also be
    \code{NULL} to install from local files.
  }
  \item{method}{
    download method, see \code{\link{download.file}}.
  }
  \item{available}{
    an object as returned by \code{\link{available.packages}}
    listing packages available at the repositories, or \code{NULL} when
    the function makes an internal call to \code{available.packages}.
  }
  \item{destdir}{
    directory where downloaded packages are stored.  If it is
    \code{NULL} (the default) a subdirectory
    \code{downloaded_packages} of the session temporary
    directory will be used (and the files will be deleted
    at the end of the session).
  }
  \item{dependencies}{logical indicating to also install uninstalled
    packages which these packages depend on/link to/import/suggest
    (and so on recursively).  Not used if \code{repos = NULL}.
    Can also be a character vector, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.

    Only supported if \code{lib} is of length one (or missing),
    so it is unambiguous where to install the dependent packages.  If
    this is not the case it is ignored, with a warning.

    The default, \code{NA}, means
    \code{c("Depends", "Imports", "LinkingTo")}.

    \code{TRUE} means to use
    \code{c("Depends", "Imports", "LinkingTo", "Suggests")} for
    \code{pkgs} and
    \code{c("Depends", "Imports", "LinkingTo")} for added dependencies:
    this installs all the packages needed to run \code{pkgs}, their
    examples, tests and vignettes (if the package author specified them
    correctly).

    In all of these, \code{"LinkingTo"} is omitted for binary packages.
  }
  \item{type}{character, indicating the type of package to download and
    install.

    Possible values are (currently) \code{"source"},
    \code{"mac.binary"}, \code{"mac.binary.mavericks"} and
    \code{"win.binary"}: the binary types can be listed and downloaded
    but not installed on other platforms.

    The default is the appropriate binary type on Windows and on the
    CRAN binary OS X distributions, otherwise \code{"source"}.  For the
    platforms where binary packages are the default, an alternative is
    \code{"both"} which means \sQuote{try binary if available and
    current, otherwise try source}.  (This will only choose the binary
    package if its version number is no older than the source version.
    In interactive use it will ask before attempting to install source
    packages.)
  }
  \item{configure.args}{
    (Used only for source installs.) A character vector or a named list.
    If a character vector with no names is supplied, the elements are
    concatenated into a single string (separated by a space) and used as
    the value for the \option{--configure-args} flag in the call to
    \command{R CMD INSTALL}.  If the character vector has names these
    are assumed to identify values for \option{--configure-args} for
    individual packages.  This allows one to specify settings for an
    entire collection of packages which will be used if any of those
    packages are to be installed.  (These settings can therefore be
    re-used and act as default settings.)

    A named list can be used also to the same effect, and that
    allows multi-element character strings for each package
    which are concatenated to a single string to be used as the
    value for \option{--configure-args}.
  }
  \item{configure.vars}{
    (Used only for source installs.) Analogous to \code{configure.args}
    for flag \option{--configure-vars}, which is used to set environment
    variables for the \command{configure} run.
  }
  \item{clean}{a logical value indicating whether to specify
    to add the \option{--clean} flag to the call to
    \command{R CMD INSTALL}.
    This is sometimes used to perform additional operations at the end
    of the package installation in addition to removing intermediate files.
  }
  \item{Ncpus}{the number of parallel processes to use for a parallel
    install of more than one source package.  Values greater than one
    are supported if the \command{make} command specified by
    \code{Sys.getenv("MAKE", "make")} accepts argument \code{-k -j
    \var{Ncpus}}.
  }
  \item{verbose}{a logical indicating if some \dQuote{progress report}
    should happen.}
  \item{libs_only}{
    a logical value: should the \option{--libs-only} option be used to
    install only additional sub-architectures?  (See also
    \code{INSTALL_opts}.)  This can also be used on Windows to install
    just the DLL(s) from a binary package, e.g. to add 64-bit DLLs to a
    32-bit install.
  }
  \item{INSTALL_opts}{
    an optional character vector of additional option(s) to be passed to
    \command{R CMD INSTALL} for a source package install.  E.g.,
    \code{c("--html", "--no-multiarch")}.

    Can also be a named list of character vectors of to be used as
    additional options, with names the respective package names.
  }
  \item{quiet}{logical: if true, reduce the amount of output.}
  \item{keep_outputs}{a logical: if true, keep the outputs from
    installing source packages in the current working directory, with
    the names of the output files the package names with \file{.out}
    appended.  Alternatively, a character string giving the directory
    where to save the outputs.  Ignored when installing from local
    files.
  }
  \item{\dots}{
    Arguments to be passed to \code{\link{download.file}} or to the
    functions for binary installs on OS X and Windows (which accept
    an argument \code{"lock"}: see the section on \sQuote{Locking}).
  }
}
\details{
  \R packages are primarily distributed as \emph{source} packages, but
  \emph{binary} packages (a packaging up of the installed package) are
  also supported, and the type most commonly used on Windows and by
  the CRAN distributions for OS X.  This function can install either
  type where supported, either by downloading a file from a repository
  or from a local file.  The default type is given by
  \code{\link{getOption}("pkgType")}: this defaults to \code{"source"}
  apart from under Windows or a CRAN binary distribution for OS X.

  This is the main function to install packages.  It takes a vector of
  names and a destination library, downloads the packages from the
  repositories and installs them.  (If the library is omitted it
  defaults to the first directory in \code{.libPaths()}, with a message
  if there is more than one.)  If \code{lib} is omitted or is of length
  one and is not a (group) writable directory, in interactive use the
  code offers to create a personal library tree (the first element of
  \code{Sys.getenv("R_LIBS_USER")}) and install there.
#ifdef windows
  Detection of a writable directory is problematic on Windows: see the
  \sQuote{Note} section.
#endif

  For source packages from a repository an attempt is made to
  install the packages in an order that respects their dependencies.
  This does assume that all the entries in \code{lib} are on the default
  library path for installs (set by \env{R_LIBS}).
#ifdef windows

  Using packages with \code{type = "source"} always works on Windows
  provided the package contains no C/C++/Fortran code that needs
  compilation.  Otherwise you will need to have installed the Rtools
  collection as described in the \sQuote{R for Windows FAQ} \emph{and}
  you must have the \env{PATH} environment variable set up as required
  by Rtools.

  When installing a binary package, \code{install.packages} will abort
  the install if it detects that the package is already installed and is
  currently in use.  In some circumstances (e.g. multiple instances of
  \R running at the same time and sharing a library) it will not detect a
  problem, but the installation may fail.
#endif

  You are advised to run \code{update.packages} before
  \code{install.packages} to ensure that any already installed
  dependencies have their latest versions.

  Argument \code{libs_only = TRUE} is supported for source installs and for
  Windows binary installs.

  For binary installs, the function also checks for the availability of
  a source package on the same repository, and reports if the source
  package has a later version, or is available but no binary version
  is.  This check can be suppressed by
\preformatted{    options(install.packages.check.source = "no")}
  and should be if there is a partial repository containing only binary
  files.
}
\value{
  Invisible \code{NULL}.
}
\section{Locking}{
  There are various options for locking: these differ between source and
  binary installs.

  By default for a source install, the library directory is
  \sQuote{locked} by creating a directory \file{00LOCK} within it.  This
  has two purposes: it prevents any other process installing into that
  library concurrently, and is used to store any previous version of the
  package to restore on error.  A finer-grained locking is provided by
  the option \option{--pkglock} which creates a separate lock for each
  package: this allows enough freedom for parallel
  installation.  Per-package locking is the default when installing a
  single package, and for multiple packages when \code{Ncpus > 1L}.
  Finally locking (and restoration on error) can be suppressed by
  \option{--no-lock}.
  % and also options(install.lock = FALSE) in an \R startup file.

  For an OS X or Windows binary install, no locking is done by
  default.  Setting argument \code{lock} to \code{TRUE} (it defaults to
  the value of \code{\link{getOption}("install.lock", FALSE)}) will use
  per-directory locking as described for source installs: if the value
  is \code{"pkglock"} per-package locking will be used.

  If package locking is used on Windows with \code{libs_only = TRUE} and
  the installation fails, the package will be restored to its previous
  state.

  Note that it is possible for the package installation to fail so badly
  that the lock directory is not removed: this inhibits any further
  installs to the library directory (or for \code{--pkglock}, of the
  package) until the lock directory is removed manually.
}

\section{Parallel installs}{
  Parallel installs are attempted if \code{pkgs} has length greater than
  one and \code{Ncpus > 1}.  It makes use of a parallel \command{make},
  so the \code{make} specified (default \command{make}) when \R was
  built must be capable of supporting \code{make -j \var{n}}: GNU make
  and \command{dmake} do, but FreeBSD and Solaris \command{make} do not:
  if necessary environment variable \env{MAKE} can be set for the
  current session to select a suitable \command{make}.

  \code{install.packages} needs to be able to compute all the
  dependencies of \code{pkgs} from \code{available}, including if one
  element of \code{pkgs} depends indirectly on another.  This means that
  if for example you are installing \acronym{CRAN} packages which depend
  on Bioconductor packages which in turn depend on \acronym{CRAN}
  packages, \code{available} needs to cover both \acronym{CRAN} and
  Bioconductor packages.
}

#ifdef unix
\note{
  Some binary distributions of \R have \code{INSTALL} in a separate
  bundle, e.g. an \code{R-devel} RPM.  \code{install.packages} will
  give an error if called with \code{type = "source"} on such a system.

  Some binary Linux distributions of \R can be installed on a machine
  without the tools needed to install packages: a possible remedy is to
  do a complete install of \R which should bring in all those tools as
  dependencies.
}
#endif
#ifdef windows
\note{
  \code{install.packages} tries to detect if you have write permission
  on the library directories specified, but Windows reports unreliably.
  If there is only one library directory (the default), \R tries to
  find out by creating a test directory, but even this need not be the
  whole story.  Under Windows Vista and later you may have permission to
  write in a library directory but lack permission to write binary files
  (such as \file{.dll} files) there.  See the \sQuote{R for Windows FAQ}
  for workarounds.
}
#endif
\seealso{
  \code{\link{update.packages}},
  \code{\link{available.packages}},
  \code{\link{download.packages}},
  \code{\link{installed.packages}},
  \code{\link{contrib.url}}.

  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  \code{\link{INSTALL}}, \code{\link{REMOVE}}, \code{\link{remove.packages}},
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}

  The \sQuote{R Installation and Administration} manual for how to
  set up a repository.
}

\examples{\dontrun{
## A Linux example for Fedora's layout
install.packages(c("ncdf4", "RNetCDF"),
  configure.args = c(RNetCDF = "--with-netcdf-include=/usr/include/udunits2"))
}}
\keyword{utilities}
