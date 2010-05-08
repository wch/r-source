% File src/library/utils/man/install.packages.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Development Team
% Distributed under GPL 2 or later

\name{install.packages}
\alias{install.packages}
\title{Install Packages from CRAN-like Repositories}
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
                 clean = FALSE, Ncpus = getOption("Ncpus"), \dots)
}
\arguments{
  \item{pkgs}{character vector of the short names of packages whose
    current versions should be downloaded from the repositories.
    
    If \code{repos = NULL}, a character vector of file paths of
#ifdef windows
    \file{.zip} files containing binary builds of packages.  With
    suitable tools installed (see the \sQuote{Details} below) source
    directories or \file{.tar.gz} archives may also be installed.
#endif
#ifdef unix
    \file{.tar.gz} files.  These can be source archives or binary
    package archive files (as created by \command{R CMD build --binary}).
    Tilde-expansion will be done on the file paths.
#endif
    
    If this is a zero-length character vector, a listbox of available
    packages is presented where possible.
  }
  \item{lib}{
    character vector giving the library directories where to
    install the packages.  Recycled as needed.  If missing, defaults to
    the first element of \code{\link{.libPaths}()}.
  }
  \item{repos}{
    character vector, the base URL(s) of the repositories
    to use, i.e., the URL of the CRAN master such as
    \code{"http://cran.r-project.org"} or its Statlib mirror,
    \code{"http://lib.stat.cmu.edu/R/CRAN"}.
    
    Can be \code{NULL} to install from local files
    (with extension \file{.tar.gz} for source packages).
  }
  \item{contriburl}{
    URL(s) of the contrib sections of the repositories.  Use this
    argument only if your repository mirror is incomplete, e.g., because
    you burned only the \file{contrib} section on a CD.  Overrides
    argument \code{repos}.  As with \code{repos}, can also be \code{NULL}
    to install from local files.
  }
  \item{method}{
    download method, see \code{\link{download.file}}.
  }
  \item{available}{
    an object as returned by \code{\link{available.packages}}
    listing packages available at the repositories, or \code{NULL} which
    makes an internal call to \code{available.packages}.
  }
  \item{destdir}{
    directory where downloaded packages are stored.  If it is
    \code{NULL} (the default) a directory
    \code{downloaded_packages} of the session temporary
    directory will be used (and the files will be deleted
    at the end of the session). 
  }
  \item{dependencies}{logical indicating to also install uninstalled
    packages on which these packages depend/suggest/import
    (and so on recursively).  Not used if \code{repos = NULL}.
    Can also be a character vector, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.

    Only supported if \code{lib} is of length one (or missing),
    so it is unambiguous where to install the dependent packages.

    The default, \code{NA}, means
    \code{c("Depends", "Imports", "LinkingTo")}
    if \code{lib} is unambiguous, and \code{FALSE} otherwise.
  }
  \item{type}{character, indicating the type of package to download and
    install.

    Possible values are \code{"source"}, \code{"mac.binary"},
    \code{"mac.binary.leopard"}, \code{"win.binary"} and
    \code{"win64.binary"}: the binary types can be listed and downloaded
    but not installed on other platforms.

    The default is the appropriate binary type on Windows and on
    the CRAN binary Mac OS X distribution, otherwise \code{"source"}.
  }
  \item{configure.args}{
    (not used on Windows) a character vector or a named list.  If a
    character vector with no names is supplied, the elements are
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
    (not used on Windows) analogous, for \option{--configure-vars} which
    is used to set environment variables for the \command{configure}
    run.
  }
  \item{clean}{a logical value indicating whether to specify
    to add the \option{--clean} flag to the call to
    \command{R CMD INSTALL}.
    This is sometimes used to perform additional operations at the end
    of the package installation in addition to removing intermediate files.
  }
  \item{Ncpus}{The number of parallel processes to use for a parallel
    install of source packages.  Values greater than one are supported
    only if GNU \command{make} is in use (more precisely, if
    \command{make -j \var{Ncpus}} works).  Defaults to \code{1} if the
    option is unset.
  }
  \item{\dots}{
    Arguments to be passed to \code{\link{download.file}}.
  }
}
\details{
  \R packages are primarily distributed as \emph{source} packages, but
  \emph{binary} packages (a packaging up of the installed package) are
  also available, and the type most commonly used by Windows and users
  of the Mac OS X GUI \code{R.app}.  \code{install.packages} can install
  either type, either by downloading a file from a repository or from a
  local file.  The default type is given by
  \code{\link{getOption}("pkgType")}: this \code{"source"} apart from
  under Windows or a CRAN binary distribution for Mac OS X.
  
  \code{install.packages} is used to install packages. It takes
  a vector of names and a destination library, downloads the packages
  from the repositories and installs them.  (If the library is omitted
  it defaults to the first directory in \code{.libPaths()}, with a
  warning if there is more than one.)  If \code{lib} is omitted or is of
  length one and is not a (group) writable directory, the code offers
  to create a personal library tree (the first element of
  \code{Sys.getenv("R_LIBS_USER")}) and install there.
#ifdef windows
  Detection of a writable directory is problematic on Windows: see the
  \sQuote{Note} section.
#endif
  
  For source packages from a repository is used an attempt is made to
  install the packages in an order that respects their dependencies.
  This does assume that all the entries in \code{lib} are on the default
  library path for installs (set by \env{R_LIBS}).
#ifdef windows
  
  Using packages with \code{type = "source"} works on Windows provided
  the package contains no C/C++/Fortran code that needs compilation,
  Otherwise you will need to have installed the Rtools collection as
  described in the \sQuote{R for Windows FAQ} \emph{and} you must have the
  \env{PATH} set up as required by Rtools.
#endif

  You are advised to run \code{update.packages} before
  \code{install.packages} to ensure that any already installed
  dependencies have their latest versions.
}
\value{
  Invisible \code{NULL}.
}

#ifdef unix
\note{
  Some binary distributions of \R have \code{INSTALL} in a separate
  bundle, e.g. an \code{R-devel} RPM.  \code{install.packages} will
  give an error if called with \code{type = "source"} on such a system.
  
  Some binary distributions can be installed on a machine without the
  tools needed to install packages: the remedy is to do a complete
  install of \R which should bring in all those tools as dependencies.
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

\examples{

\dontrun{
install.packages(
    c("XML_0.99-5.tar.gz",
      "../../Interfaces/Perl/RSPerl_0.8-0.tar.gz"),
    repos = NULL,
    configure.args = c(XML = '--with-xml-config=xml-config',
                       RSPerl = "--with-modules='IO Fcntl'"))
}
}
\keyword{utilities}
