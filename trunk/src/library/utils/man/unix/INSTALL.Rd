\name{INSTALL}
\alias{INSTALL}
\title{Install Add-on Packages}
\description{Utility for installing add-on packages.}
\usage{
\special{R CMD INSTALL [options] [-l lib] pkgs}
}
\arguments{
  \item{pkgs}{A space-separated list with the path names of the packages to be
    installed.}
  \item{lib}{the path name of the \R library tree to install to.}
  \item{options}{a space-separated list of options through which in
    particular the process for building the help files can be
    controlled.  Options should only be given once.
    Use \code{R CMD INSTALL --help} for the current list of options.
  }
}
\details{
  This will stop at the first error, so if you want all the \code{pkgs}
  to be tried, call this via a shell \code{for} or \code{foreach} loop.

  If used as \code{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory in the library path which would be used by \R run in
  the current environment.

  To install into the library tree \code{lib}, use
  \code{R CMD INSTALL -l lib pkgs}.  This prepends \code{lib} to
  \env{R\_LIBS} for duration of the install, so required packages in the
  installation directory will be found (and used in preference to those
  in other libraries).

  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names of directories.  \code{pkgs} may also contain
  names of package/bundle archive files of the form
  \file{pkg\_version.tar.gz} as obtained from CRAN: these are then
  extracted in a temporary directory.  Finally, binary package/bundle
  archive files (as created by \code{R CMD build --binary} can be supplied.

  Some package sources contain a \file{configure} script that can be
  passed arguments or variables via the option \option{--configure-args}
  and \option{--configure-vars}, respectively, if necessary.  The latter
  is useful in particular if libraries or header files needed for the
  package are in non-system directories.  In this case, one can use the
  configure variables \code{LIBS} and \code{CPPFLAGS} to specify these
  locations (and set these via \option{--configure-vars}), see section
  \dQuote{Configuration variables} in \dQuote{R Installation and
    Administration} for more information.  (If these are used more than
  once on the command line, only the last instance is used.)  One can
  bypass the configure mechanism using the option \option{--no-configure}.

  If \option{--no-docs} is given, no help files are built.  Options
  \option{--no-text}, \option{--no-html}, and \option{--no-latex} suppress
  creating the text, HTML, and LaTeX versions, respectively.  The
  default is to build help files in all three versions.

  If the attempt to install the package fails, leftovers are removed.
  If the package was already installed, the old version is restored.
  This happens either if a command encounters an error or if the
  install is interrupted from the keyboard: after cleaning up the script
  terminates.

  Use \code{R CMD INSTALL --help} for more usage information.
}

\section{Packages using the methods package}{
  Packages that require the methods package, and that use functions such
  as \code{\link{setMethod}} or \code{\link{setClass}},
  should be installed either (preferably) using lazy-loading or by
  creating a binary image: use the flags \code{LazyLoad} and
  \code{SaveImage} in the \file{DESCRIPTION} file to ensure this.
}

\seealso{
  \code{\link{REMOVE}} and \code{\link{library}} for information on
  using several library trees;
  \code{\link{update.packages}} for automatic update of packages using
  the internet (or other \R level installation of packages, such as by
  \code{install.packages}).

  The section on \dQuote{Add-on packages} in \dQuote{R Installation and
    Administration} and the chapter on \dQuote{Creating R packages} in
  \dQuote{Writing \R Extensions} (see \code{\link{RShowDoc}} and the
  \file{doc/manual} subdirectory of the \R source tree).
}
\keyword{utilities}
