\name{update.packages}
\alias{update.packages}
\alias{available.packages}
\alias{old.packages}
\alias{new.packages}
\alias{download.packages}
\alias{install.packages}
\alias{contrib.url}
\title{Download Packages from CRAN-like repositories}
\description{
  These functions can be used to automatically compare the version
  numbers of installed packages with the newest available version on
  the repositories and update outdated packages on the fly.
}
\usage{
update.packages(lib.loc = NULL, repos = getOption("repos"),
                contriburl = contrib.url(repos, type),
                method, instlib = NULL,
                ask = TRUE, available = NULL, destdir = NULL,
		installWithVers = FALSE, checkBuilt = FALSE,
                type = getOption("pkgType"))

available.packages(contriburl = contrib.url(getOption("repos")), method)

old.packages(lib.loc = NULL, repos = getOption("repos"),
             contriburl = contrib.url(repos),
             method, available = NULL, checkBuilt = FALSE)

new.packages(lib.loc = NULL, repos = getOption("repos"),
             contriburl = contrib.url(repos),
             method, available = NULL, ask = FALSE)

download.packages(pkgs, destdir, available = NULL,
                  repos = getOption("repos"),
                  contriburl = contrib.url(repos, type),
                  method, type = getOption("pkgType"))

install.packages(pkgs, lib, repos = getOption("repos"),
                 contriburl = contrib.url(repos, type),
                 method, available = NULL, destdir = NULL,
		 installWithVers = FALSE, dependencies = FALSE,
                 type = getOption("pkgType"))

contrib.url(repos, type = getOption("pkgType"))
}
\arguments{
  \item{lib.loc}{character vector describing the location of R
    library trees to search through (and update packages therein).}
  \item{repos}{character vector, the base URL(s) of the repositories
    to use, i.e., the URL of the CRAN master such as
    \code{"http://cran.r-project.org"} or its Statlib mirror,
    \code{"http://lib.stat.cmu.edu/R/CRAN"}.
#ifdef unix
    Can be \code{NULL} to install from local \code{.tar.gz} files.
#endif
#ifdef windows
    Can be \code{NULL} to install from local \code{.zip} files.
#endif
  }
  \item{contriburl}{URL(s) of the contrib section of the
    repositories. Use this argument only if your CRAN mirror is
    incomplete, e.g., because you burned only the \file{contrib} section on a
    CD.  Overrides argument \code{repos}.
#ifdef unix
    Can be \code{NULL} to install from local \code{.tar.gz} files.
#endif
#ifdef windows
    Can also be \code{NULL} to install from local \code{.zip} files.
#endif
  }
  \item{method}{Download method, see \code{\link{download.file}}.}
  \item{pkgs}{character vector of the short names of packages/bundles whose
    current versions should be downloaded from the repositories.
    If \code{repos = NULL}, a character vector of file paths of
#ifdef windows
    \code{.zip} files.
#endif
#ifdef unix
    \code{.tar.gz} files.  These can be source archives or binary
    package/bundle archive files (as created by \code{R CMD build --binary}).
#endif
    If this is a zero-length character vector, a listbox of available
    packages (including those contained in bundles) is presented where
    possible.
  }
  \item{destdir}{directory where downloaded packages are stored.}
  \item{available}{an object listing packages available at the repositories
    as returned by \code{available.packages}.}
  \item{lib}{character vector giving the library directories where to
    install the packages.  Recycled as needed.}
  \item{ask}{logical indicating whether to ask user before packages
    are actually downloaded and installed, or the character string
    \code{"graphics"}, which brings up a widget to allow the user to
    (de-)select from the list of packages which could be updated.  The
    latter only works on systems with a GUI version of
    \code{\link{select.list}}, and is otherwise equivalent to \code{ask
      = TRUE}.
  }
  \item{installWithVers}{If \code{TRUE}, will invoke the install of the
    package such that it can be referenced by package version.}
  \item{checkBuilt}{If \code{TRUE}, a package built under an earlier
    minor version of \R is considered to be \sQuote{old}.}
  \item{instlib}{character string giving the library directory where to
    install the packages.}
  \item{dependencies}{logical indicating to also install uninstalled
    packages on which these packages depend/suggest/import
    (and so on recursively).  Not used if \code{repos = NULL}.
    Can also be a character vector, a subset of
    \code{c("Depends", "Imports", "Suggests")}.
  }
  \item{type}{character, indicating the type of package to download and
    install.
#ifdef unix
    Possible values are \code{"source"} (the default except under the
    CRAN Mac OS X build), \code{"mac.binary"} and \code{"win.binary"}
    (which can be downloaded but not installed).
#endif
#ifdef windows
    Possible values are \code{"win.binary"} (the default) and
    \code{"source"} (for which suitable tools need to be installed: see
    the \sQuote{rw-FAQ}).  Value \code{"mac.binary"} can be used to
    explore and download Mac OS X binaries.
#endif
  }
}
\details{
  All of these functions work with the names of a package or bundle (and
  not the component packages of a bundle, except for
  \code{install.packages} if the repository provides the necessary
  information).
  
  \code{available.packages} returns a matrix of details corresponding to
  packages/bundles currently available at  one or more repositories. The
  current list of packages is downloaded over the internet (or copied
  from a local mirror).  It returns only packages whose version
  requirements are met by the running version of \R.
  
  \code{old.packages} compares the information from
  \code{available.packages} with that from
  \code{\link{installed.packages}} and reports installed
  packages/bundles that have newer versions on the repositories or, if
  \code{checkBuilt = TRUE}, that were built under an earlier minor
  version of \R (for example built under 2.0.x when running \R 2.1.1).
#ifdef windows
  (There is no check that the version on the repository was built under
  the current minor version of \R, but it is advertised as being
  suitable for this version.)
#endif

  \code{new.packages} does the same comparison but reports uninstalled
  packages/bundles that are available at the repositories.  It will also
  give warnings about incompletely installed bundles (provided the
  information is available) and bundles whose contents has changed.
  If \code{ask != FALSE} it asks which packages should be installed
  in the first element of \code{lib.loc}.

  \code{download.packages} takes a list of package/bundle names and a
  destination directory, downloads the newest versions and saves them in
  \code{destdir}.  If the list of available packages is not given as
  argument, it is obtained from repositories.  If a repository is local,
  i.e., the URL starts with \code{"file:"}, then the packages are not
  downloaded but used directly.  (Both \code{"file:"} and
  \code{"file:///"} are allowed as prefixes to a file path, the latter
  for an absolute file path.)

  The main function of the set is \code{update.packages}.  First a list
  of all packages/bundles found in \code{lib.loc} is created and
  compared with those available at the repostories.  If \code{ask =
    TRUE} (the default) packages/bundles with a newer version are
  reported and for each one the user can specify if it should be
  updated.
#ifdef unix
  If so, the package sources are downloaded from the repositories and
  installed in the respective library path (or \code{instlib}
  if specified) using the \R \code{\link{INSTALL}} mechanism.
#endif
#ifdef windows
  If so, the pre-compiled packages are downloaded from the repositories
  and installed in the respective library path (or \code{instlib} if
  specified).
#endif

  \code{install.packages} can be used to install new packages/bundles.
  It takes a vector of names and a destination library, downloads the
  packages from the repositories and installs them.  (If the library is
  omitted it defaults to the first directory in \code{.libPaths()}, with
  a warning if there is more than one.)
%  The first argument can be an unquoted name, as in
%  \code{install.packages(nlme)}.
#ifdef windows
  Argument \code{pkgs} can also be a character vector of file names of
  zip files if \code{repos = NULL}. The zip files are then unpacked directly.
#endif
#ifdef unix
  An attempt is made to install the packages in an order that respects
  their dependencies.  This does assume that all the entries in
  \code{lib} are on the default library path for installs (set by
  \code{R_LIBS}). 
#endif

  \code{contrib.url} adds the appropriate type-specific path within a
  repository to each URL in \code{repos}.

  For \code{install.packages} and \code{update.packages}, \code{destdir}
  is the directory to which packages will be downloaded.  If it is
  \code{NULL} (the default) a directory \code{downloaded_packages} of
  the session temporary directory will be used (and the files will be
  deleted at the end of the session).
  
  If \code{repos} or \code{contriburl} is a vector of length greater than
  one, the newest version of the package is fetched from the first
  repository on the list within which it is found.
}
\value{
  For \code{available.packages}, a matrix
  with one row per package/bundle, row names the package names and
  column names \code{"Package"}, \code{"Version"}, \code{"Priority"},
  \code{"Bundle"}, \code{"Depends"}, \code{"Imports"}, \code{"Suggests"}
  \code{"Contains"} and \code{"Repository"}.

  For \code{old.packages}, \code{NULL} or a matrix with one row per
  package/bundle, row names the package names and column names
  \code{"Package"}, \code{"LibPath"}, \code{"Installed"} (the version),
  \code{"Built"} (the version built under), \code{"ReposVer"} and
  \code{"Repository"}.
  
  For \code{new.packages} a character vector of package/bundle names,
  \emph{after} any have been installed.

  For \code{download.packages}, a two-column matrix of names and
  destination file names, for those packages/bundles successfully downloaded.

  \code{install.packages} and \code{update.packages} have no return value.
}
\section{Warning}{
  Not enough information is recorded to know if a bundle is completely
  installed, so a bundle is regarded as installed if any of its
  component packages is.
}
\note{
  Some binary distributions of \R have \code{INSTALL} in a separate
  bundle, e.g. an \code{R-devel} RPM.  \code{install.packages} will
  give an error if called on such a system.
}
\seealso{
  \code{\link{installed.packages}}.
  
  See \code{\link{download.file}} for how to handle proxies and
  other options to monitor file transfers.

  \code{\link{INSTALL}}, \code{\link{REMOVE}},
  \code{\link{library}}, \code{\link{.packages}}, \code{\link{read.dcf}}
}
\keyword{utilities}
