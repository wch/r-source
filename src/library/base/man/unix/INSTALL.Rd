\name{INSTALL}
\title{Install Add-on Packages}
\alias{INSTALL}
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
  To install packages into the default library tree (which is rooted at
  \file{\$R\_HOME/library}), do \code{R CMD INSTALL pkgs}.  To install
  into the library tree \code{lib} instead of the default one, use
  \code{R CMD INSTALL -l lib pkgs}.

  Both \code{lib} and the elements of \code{pkgs} may be absolute or
  relative path names. \code{pkgs} can also contain name of package
  archive files of the form \file{pkg\_version.tar.gz} as obtained from
  CRAN, these are then extracted in a temporary directory.
  
  Some package sources contain a \file{configure} script that can be
  passed arguments via the option \code{--configure-args}, if
  necessary.  One can also bypass the configure mechanism using the
  option \code{--no-configure}.
  
  If \code{--no-docs} is given, no help files are built.  Options
  \code{--no-text}, \code{--no-html}, and \code{--no-latex} suppress
  creating the text, HTML, and LaTeX versions, respectively.  The
  default is to build help files in all three versions.

  Use \code{R CMD INSTALL --help} for more usage information.
}
\seealso{
  \code{\link{REMOVE}},
  \code{\link{update.packages}} for automatic update of packages using
  the internet;
  the chapter on ``Creating R packages'' in ``Writing \R Extensions''
  (see the \file{doc/manual} subdirectory of the \R source tree).
}
\keyword{utilities}
