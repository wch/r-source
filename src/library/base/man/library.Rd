\name{library}
\title{Loading and Listing of Packages}
\usage{
library(name, help = NULL, lib.loc = .lib.loc,
	character.only = FALSE, logical.return = FALSE)
require(name, quietly = FALSE)
provide(name)

library.dynam(chname)

.packages()
.lib.loc
.Library
.Provided
}
\alias{library}
\alias{library.dynam}
\alias{provide}
\alias{require}
\alias{.lib.loc}
\alias{.Library}
\alias{.Provided}
\alias{RLIBS}
\arguments{
 \item{name, help}{\code{name} or character string giving the name of a
  package}
 \item{lib.loc}{a character vector describing the location of \R library
  trees to search through.}
 \item{character.only}{a logical indicating whether \code{name} or
  \code{help} can be assumed to be character strings}
 \item{quietly}{if \code{TRUE}, a warning will not be printed if the
  package cannot be found.}
 \item{chname}{a character string naming a shared library to load}
}
\description{
  \code{library(name)} and \code{require(name)} both load the package
  with name \code{name}.
  \code{require} is designed for use inside other functions; it returns
  \code{FALSE} and optionally gives a warning, rather than giving an
  error, if the package does not exist.
  Both functions check and update the list of currently loaded packages
  and do not reload code that is already loaded.  \code{require} also
  checks the list \code{.Provided}.

  \code{provide} allows code to register services that it provides.  The
  argument is stored in the list \code{.Provided}.  \code{provide}
  returns \code{FALSE} if the name was already present in
  \code{.Provided} or among the packages in \code{search()}.  The main
  use for \code{provide} is when multiple packages share code.	This is
  most likely when the code implements features present in S(-PLUS) but
  not in R. For example, the spline functions \code{ns}, \code{bs} and
  so on are not included in the \R distribution.  A package containing
  these functions can use \code{provide(splines)} to register this fact.
  Another package that needs the functions can execute
  \code{require(splines)} rather than \code{library(splines)} to load
  the spline package only if their functionality is not already available.

  If \code{library} is called with no argument, it gives a list of all
  available packages.  \code{library(help = name)} prints information on
  the package \code{name}, typically by listing the most important user
  level objects it contains.

  \code{library.dynam} loads the specified (shared) object file if it
  has not been loaded already.	It is designed to be used inside a
  package rather than at the command line.  The system-specific
  extension for shared libraries (e.g., ``.so'' on Unix systems) should
  not be added.

  \code{.packages()} returns the ``base names'' of the currently attached
  packages.

  \code{.Library} is a character string giving the location of the
  default library, the ``library'' subdirectory of \code{RHOME}.
  \code{.lib.loc} is a character vector with the locations of all
  library trees that \R should use.  It is initialized at startup from
  the environment variable \code{RLIBS}, which should be a
  colon-separated list of directories at which \R library trees are
  rooted, and \code{.Library}.
}

\section{Creating Packages}{
  Packages provide a mechanism for loading optional code and its
  documentation as needed.  The \R distribution provides the two example
  packages \code{eda} and \code{mva}.

  A package consists of a subdirectory containing a \file{TITLE} and
  \file{INDEX} file, and subdirectories \file{R}, \file{man} and
  optionally \file{src}, \file{src-c}, \file{data}, and \file{exec}.

  The \file{TITLE} file contains a line giving the name of the package
  and a brief description.  \file{INDEX} contains a line for each
  sufficiently interesting object in the package, giving its name and a
  description (functions such as print methods not usually called
  explicitly might not be included).

  The \file{R} subdirectory contains \R code files with names beginning
  with lowercase letters.  One of these files should use
  \code{library.dynam()} to load any necessary compiled code.

  The \file{man} subdirectory should contain \R documentation files for
  the objects in the package.

  Source and a Makefile for the compiled code is in \file{src}, and a
  pure C version of the source should be in \file{src-c}.  In the common
  case when all the source is in C it may be convenient to make one of
  these directories a symbolic link to the other. The Makefile will be
  passed various machine-dependent compile and link flags, examples of
  which can be seen in the \code{eda} package.

  The \file{data} subdirectory is for additional data files the package
  makes available for loading using \code{data()}.  Note that (at least
  currently) all such files are in fact R code files, and must have the
  extension `.R'.

  Finally, \file{exec} could contain executables, typically (shell or
  Perl) scripts, the package needs.  Note that this mechanism currently
  only is experimental.
}
\section{Installing and Removing Packages}{
  To install a package, do \code{R INSTALL pkg}, where \code{pkg} is the
  directory containing the package.  If you want to install into the
  library tree \code{lib} instead of the default one, use
  \code{R INSTALL pkg lib}.

  To remove the package \code{pkg} from the default library or the
  library \code{lib}, do \code{R REMOVE pkg} or \code{R REMOVE pkg lib},
  respectively.
}
\value{
  \code{library} returns the list of loaded packages (or \code{TRUE} if
  \code{logical.return} is \code{TRUE}).
  \code{require} returns a logical indicating whether the required
  package is available.
}
\seealso{
  \code{\link{attach}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{autoload}}.
}
\examples{
library()		    # list all available packages
library(lib = .Library)	    # list all packages in the default library
library(help = eda)	    # documentation on package "eda"
library(eda)		    # load package "eda"
require(eda)		    # the same
require(nonexistent)	    # FALSE
}
\keyword{data}
