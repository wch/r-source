\name{INSTALL}
\alias{INSTALL}
\title{Install Add-on Packages}
\description{Utility for installing add-on packages.}
\usage{R CMD INSTALL [options] [-l lib] pkgs}
\arguments{
  \item{pkgs}{A list with the path names of the packages to be
    installed.}
  \item{lib}{the path name of the \R library tree to install to.}
  \item{options}{a list of options through which in particular the
    process for building the help files can be controlled.}
}
\details{
  If used as \code{R CMD INSTALL pkgs} without explicitly specifying
  \code{lib}, packages are installed into the library tree rooted at the
  first directory given in the environment variable \code{R\_LIBS} if
  this is set and non-null, and to the default library tree (which is
  rooted at \file{\$R\_HOME/library}) otherwise.

  To install into the library tree \code{lib}, use
  \code{R CMD INSTALL -l lib pkgs}.

  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names. \code{pkgs} can also contain name of package
  archive files of the form \file{pkg\_version.tar.gz} as obtained from
  CRAN, these are then extracted in a temporary directory.
  
  Some package sources contain a \file{configure} script that can be
  passed arguments or variables via the option \code{--configure-args}
  and \code{--configure-vars}, respectively, if necessary.  The latter
  is useful in particular if libraries or header files needed for the
  package are in non-system directories.  In this case, one can use the
  configure variables \code{LIBS} and \code{CPPFLAGS} to specify these
  locations (and set these via \code{--configure-vars}), see section
  `Configuration variables' in ``R Installation and Administration'' for
  more information.  One can also bypass the configure mechanism using
  the option \code{--no-configure}.
  
  If \code{--no-docs} is given, no help files are built.  Options
  \code{--no-text}, \code{--no-html}, and \code{--no-latex} suppress
  creating the text, HTML, and LaTeX versions, respectively.  The
  default is to build help files in all three versions.

  If the option \code{--save} is used, the installation procedure
  creates a binary image of the package code, which is then loaded when
  the package is attached, rather than evaluating the package source at
  that time.  Having a file \file{install.R} in the package directory
  makes this the default behavior for the package (option
  \code{--no-save} overrides).  You may need  \code{--save} if your
  package requires other packages to evaluate its own source.  If the
  file \file{install.R} is non-empty, it should contain R expressions to
  be executed when the package is attached, after loading the saved
  image.  Options to be passed to R when creating the save image can be
  specified via \code{--save=ARGS}.

  Use \code{R CMD INSTALL --help} for more usage information.
}

\section{Packages using the methods package}{
  Packages that require the methods package, and that use functions such
  as \code{\link[methods]{setMethod}} or
  \code{\link[methods]{setClass}}, should be installed by creating a
  binary image.

  The package should require the methods package, both during
  installation and when the user attaches the package.  A good solution
  for most cases is to include the line \code{require("methods")} twice,
  once at the beginning of the package's R source, and once in the file
  \file{install.R} in the package directory (the top-level directory,
  \emph{not} in the \file{R} directory below that).  The
  \file{install.R} file causes an image to be saved, and the contents
  will ensure that methods are available when the package is attached.

}
\seealso{
  \code{\link{REMOVE}},
  \code{\link{update.packages}} for automatic update of packages using
  the internet;
  the chapter on ``Creating R packages'' in ``Writing \R Extensions''
  (see the \file{doc/manual} subdirectory of the \R source tree).
}
\keyword{utilities}
