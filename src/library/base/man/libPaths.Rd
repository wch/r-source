\name{libPaths}
\title{Search Paths for Packages}
\alias{.Library}
\alias{.Library.site}
\alias{.libPaths}
\alias{R_LIBS}
\alias{R_LIBS_SITE}
\alias{R_LIBS_USER}
\alias{.expand_R_libs_env_var}
\description{
  \code{.libPaths} gets/sets the library trees within which packages are
  looked for.
}
\usage{
.libPaths(new)

.Library
.Library.site
}
\arguments{
  \item{new}{a character vector with the locations of \R library
    trees.  Tilde expansion (\code{\link{path.expand}}) is done, and if
    any element contains one of \code{*?[}, globbing is done where
    supported by the platform: see \code{\link{Sys.glob}}.}
}
\details{
  \code{.Library} is a character string giving the location of the
  default library, the \file{library} subdirectory of \env{R\_HOME}.

  \code{.Library.site} is a (possibly empty) character vector giving the
  locations of the site libraries, by default the \file{site-library}
  subdirectory of \env{R\_HOME} (which may not exist).

  \code{.libPaths} is used for getting or setting the library trees that
  \R knows about (and hence uses when looking for packages).  If called
  with argument \code{new}, the library search path is set to
  the existing directories in \code{unique(c(new, .Library.site, .Library))}
  and this is returned.  If given no argument, a character vector with
  the currently active library trees is returned.

#ifdef unix
  The library search path is initialized at startup from the environment
  variable \env{R\_LIBS} (which should be a colon-separated list of
  directories at which \R library trees are rooted) followed by those in
  environment variable \env{R\_LIBS\_USER}.  Only directories which exist
  at the time will be included.

  By default \env{R\_LIBS} is unset, and \env{R\_LIBS\_USER} is set to
  directory \file{R/\var{R.version\$platform}-library/\var{x.y}}
  of the home directory, for \R \var{x.y.z}.

  \code{.Library.site} can be set via the environment variable 
  \env{R\_LIBS\_SITE} (as a colon-separated list of library trees).
#endif
#ifdef windows
  The library search path is initialized at startup from the environment
  variable \env{R\_LIBS} (which should be a semicolon-separated list of
  directories at which \R library trees are rooted) followed by those in
  environment variable \env{R\_LIBS\_USER}.  Only directories which exist
  at the time will be included.

  By default \env{R\_LIBS} is unset, and \env{R\_LIBS\_USER} is set to
  subdirectory \file{R/win-library/\var{x.y}} of the home directory,
  for \R \var{x.y.z}.

  \code{.Library.site} can be set via the environment variable
  \env{R\_LIBS\_SITE} (as a semicolon-separated list of library trees).
#endif

  Both \env{R\_LIBS\_USER} and \env{R\_LIBS\_SITE} feature possible
  expansion of specifiers for \R version specific information as part of
  the startup process.  The possible conversion specifiers all start
  with a \samp{\%} and are followed by a single letter (use \samp{\%\%}
  to obtain \samp{\%}), with currently available conversion
  specifications as follows:
  \describe{
    \item{\code{\%V}}{\R version number including the patchlevel (e.g.,
      \samp{2.5.0}).}
    \item{\code{\%v}}{\R version number excluding the patchlevel (e.g.,
      \samp{2.5}).}
    \item{\code{\%p}}{the platform for which \R was built.}
    \item{\code{\%o}}{the underlying operating system.}
    \item{\code{\%a}}{the architecture (CPU) \R was built on/for.}
  }
  (See \code{\link{version}} for details on R version information.)

  Function \code{.libPaths} always uses the values of \code{.Library}
  and \code{.Library.site} in the base name space.  \code{.Library.site}
  can be set by the site in \file{Rprofile.site}, which should be
  followed by a call to \code{.libPaths(.libPaths())} to make use of the
  updated value.
}
\value{
  A character vector of file paths.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{library}}
}
\examples{
.libPaths()                 # all library trees R knows about
}
\keyword{data}
