\name{INSTALL}
\alias{INSTALL}
\title{Install Add-on Packages}
\description{Utility for installing add-on packages.}
\usage{R CMD INSTALL [options] [-l lib] pkgs}
\arguments{
  \item{pkgs}{A list with the path names of the packages to be
    installed.}
  \item{lib}{the path name of the \R library tree to install to.}
  \item{options}{a space-separated list of options through which in
    particular the process for building the help files can be
    controlled.  Options should only be given once.
    Use \code{R CMD INSTALL --help} for the current list of options.
  }
}
\details{
  If used as \code{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory given in the environment variable \env{R\_LIBS} if
  this is set and non-null, and to the default library tree (which is
  rooted at \file{\$R\_HOME/library}) otherwise.

  To install into the library tree \code{lib}, use
  \code{R CMD INSTALL -l lib pkgs}.

  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names. \code{pkgs} can also contain name of package
  archive files of the form \file{pkg\_version.tar.gz} as obtained from
  CRAN, these are then extracted in a temporary directory.
  
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

  If the option \option{--save} is used, the installation procedure
  creates a binary image of the package code, which is then loaded when
  the package is attached, rather than evaluating the package source at
  that time.  Having a file \file{install.R} in the package directory
  makes this the default behavior for the package (option
  \option{--no-save} overrides).  You may need  \option{--save} if your
  package requires other packages to evaluate its own source.  If the
  file \file{install.R} is non-empty, it should contain R expressions to
  be executed when the package is attached, after loading the saved
  image.  Options to be passed to R when creating the save image can be
  specified via \option{--save=ARGS}.

  Options \option{--lazy}, \option{--no-lazy}, \option{--lazy-data} and
  \option{--no-lazy-data} control where the \R objects and the datasets
  are made available for lazy loading.  (These options are overridden by
  any values set in the DESCRIPTION file.)  The default is
  \option{--no-lazy --no-lazy-data} except that lazy-loading is used for
  package with more than 50kB of \R code and no saved image.

  If the attempt to install the package fails, leftovers are removed.
  If the package was already installed, the old version is restored.

  Use \code{R CMD INSTALL --help} for more usage information.
}

\section{Packages using the methods package}{
  Packages that require the methods package, and that use functions such
  as \code{\link{setMethod}} or \code{\link{setClass}},
  should be installed by creating a binary image.

  The presence of a file named
  \file{install.R} in the package's main directory causes an image to
  be saved.  Note that the file is not in the \file{R} subdirectory: all
  the code in that subdirectory is used to construct the binary image.

  Normally, the file \file{install.R} will be empty; if it does contain
  R expressions
  these will be evaluated when the package is attached, e.g. by a call to
  the function \code{\link{library}}.  (Specifically, the source code
  evaluated for a package with a saved image consists of a suitable
  definition of \code{.First.lib} to ensure loading of the saved
  image, followed by the R code in file \file{install.R}, if any.)

}
\seealso{
  \code{\link{REMOVE}} and \code{\link{library}} for information on
  using several library trees;
  \code{\link{update.packages}} for automatic update of packages using
  the internet;
  the chapter on \dQuote{Creating R packages} in \dQuote{Writing \R
    Extensions} (see the \file{doc/manual} subdirectory of the \R source
  tree).
}
\keyword{utilities}
