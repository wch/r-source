% File src/library/base/man/libPaths.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Distributed under GPL 2 or later

\name{libPaths}
\title{Search Paths for Packages}
\alias{.Library}
\alias{.Library.site}
\alias{.libPaths}
\alias{R_LIBS}
\alias{R_LIBS_SITE}
\alias{R_LIBS_USER}
\description{
  \code{.libPaths} gets/sets the library trees within which packages are
  looked for.
}
\usage{
.libPaths(new, include.site = TRUE)

.Library
.Library.site
}
\arguments{
  \item{new}{a character vector with the locations of \R library
    trees.  Tilde expansion (\code{\link{path.expand}}) is done, and if
    any element contains one of \code{*?[}, globbing is done where
    supported by the platform: see \code{\link{Sys.glob}}.}
  \item{include.site}{a logical value indicating whether the value of
    \code{.Library.site} should be included in the new set of library
    tree locations.  Defaulting to \code{TRUE}, it is ignored when
    \code{.libPaths} is called without the \code{new} argument.}
}
\details{
  \code{.Library} is a character string giving the location of the
  default library, the \file{library} subdirectory of \var{\link{R_HOME}}.

  \code{.Library.site} is a (possibly empty) character vector giving the
  locations of the site libraries.

  \code{.libPaths} is used for getting or setting the library trees that \R
  knows about and hence uses when looking for packages (the library search
  path).  If called with argument \code{new}, by default, the library search
  path is set to the existing directories in \code{unique(c(new,
  .Library.site, .Library))} and this is returned.  If \code{include.site}
  is \code{FALSE} when the \code{new} argument is set, \code{.Library.site}
  is not added to the new library search path.  If called without the
  \code{new} argument, a character vector with the currently active library
  trees is returned.

  How paths in \code{new} with a trailing slash are treated is
  OS-dependent.  On a POSIX filesystem existing directories can usually
  be specified with a trailing slash. On Windows filepaths with a
  trailing slash (or backslash) are invalid and existing directories
  specified with a trailing slash may not be added to the library search path.

  At startup, the library search path is initialized from the
  environment variables \env{R_LIBS}, \env{R_LIBS_USER} and
  \env{R_LIBS_SITE}, which if set should give lists of directories where
  \R library trees are rooted, colon-separated on Unix-alike systems and
  semicolon-separated on Windows.  For the latter two, a value of
  \code{NULL} indicates an empty list of directories. (Note that as from
  \R 4.2.0, both are set by \R start-up code if not already set or empty
  so can be interrogated from an \R session to find their defaults:
  in earlier versions this was true only for \env{R_LIBS_USER}.)

  First, \code{.Library.site} is initialized from \env{R_LIBS_SITE}.  If
  this is unset or empty, the \file{site-library} subdirectory of
  \env{R_HOME} is used.  Only directories which exist at the time of
  initialization are retained.  Then, \code{.libPaths()} is called with
  the combination of the directories given by \env{R_LIBS} and
  \env{R_LIBS_USER}.  By default \env{R_LIBS} is unset, and if
  \env{R_LIBS_USER} is unset or empty, it is set to directory
  \file{R/\var{R.version$platform}-library/\var{x.y}} of the home
  directory on Unix-alike systems (or
  \file{Library/R/\var{m}/\var{x.y}/library} for CRAN macOS builds, with
  \var{m} \code{\link{Sys.info}()["machine"]}) and
  \file{R/win-library/\var{x.y}} subdirectory of \env{LOCALAPPDATA} on
  Windows, for \R \var{x.y.z}.

  Both \env{R_LIBS_USER} and \env{R_LIBS_SITE} feature possible
  expansion of specifiers for \R-version-specific information as part of
  the startup process.  The possible conversion specifiers all start
  with a \samp{\%} and are followed by a single letter (use \samp{\%\%}
  to obtain \samp{\%}), with currently available conversion
  specifications as follows:
  \describe{
    \item{\samp{\%V}}{\R version number including the patch level (e.g.,
      \samp{2.5.0}).}
    \item{\samp{\%v}}{\R version number excluding the patch level (e.g.,
      \samp{2.5}).}
    \item{\samp{\%p}}{the platform for which \R was built, the value of
      \code{\link{R.version}$platform}.}
    \item{\samp{\%o}}{the underlying operating system, the value of
      \code{\link{R.version}$os}.}
    \item{\samp{\%a}}{the architecture (CPU) \R was built on/for, the
      value of \code{\link{R.version}$arch}.}
  }
  (See \code{\link{version}} for details on R version information.)
  In addition, \samp{\%U} and \samp{\%S} expand to the \R defaults for,
  respectively, \env{R_LIBS_USER} and \env{R_LIBS_SITE}.

  Function \code{.libPaths} always uses the values of \code{.Library}
  and \code{.Library.site} in the base namespace.  \code{.Library.site}
  can be set by the site in \file{Rprofile.site}, which should be
  followed by a call to \code{.libPaths(.libPaths())} to make use of the
  updated value.

  For consistency, the paths are always normalized by
  \code{\link{normalizePath}(winslash = "/")}.

  \env{LOCALAPPDATA} (usually \code{C:\\Users\\username\\AppData\\Local}) on
  Windows is a hidden directory and may not be viewed by some software. It
  may be opened by \code{shell.exec(Sys.getenv("LOCALAPPDATA"))}.
}
\value{
  A character vector of file paths.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{library}}
}
\examples{
.libPaths()                 # all library trees R knows about
}
\keyword{data}
