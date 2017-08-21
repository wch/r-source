% File src/library/utils/man/package.skeleton.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{package.skeleton}
\alias{package.skeleton}
\title{Create a Skeleton for a New Source Package}
\description{
  \code{package.skeleton} automates some of the setup for a new source
  package.  It creates directories, saves functions, data, and R code files to
  appropriate places, and creates skeleton help files and a
  \file{Read-and-delete-me} file describing further steps in packaging.
}
\usage{
package.skeleton(name = "anRpackage", list,
                 environment = .GlobalEnv,
                 path = ".", force = FALSE,
                 code_files = character(), encoding = "unknown")
}
\arguments{
  \item{name}{character string: the package name and directory name for
    your package.}
  \item{list}{character vector naming the \R objects to put in the
    package.  Usually, at most one of \code{list}, \code{environment},
    or \code{code_files} will be supplied.  See \sQuote{Details}.}
  \item{environment}{an environment where objects are looked for.  See
    \sQuote{Details}.}
  \item{path}{path to put the package directory in.}
  \item{force}{If \code{FALSE} will not overwrite an existing directory.}
  \item{code_files}{a character vector with the paths to R code files to
    build the package around.  See \sQuote{Details}.}
  \item{encoding}{optionally a \code{\link{character}} string with an
    encoding for an optional \code{Encoding:} line in
    \file{DESCRIPTION} when non-ASCII characters will be used; typically
    one of \code{"latin1"}, \code{"latin2"}, or \code{"UTF-8"}; see the
    WRE manual.}
}
\value{
  Used for its side-effects.
}
\details{
  The arguments \code{list}, \code{environment}, and \code{code_files}
  provide alternative ways to initialize the package.  If
  \code{code_files} is supplied, the files so named will be sourced to
  form the environment, then used to generate the package skeleton.
  Otherwise \code{list} defaults to the objects in \code{environment}
  (including those whose names start with \code{.}), but can be supplied
  to select a subset of the objects in that environment.

  Stubs of help files are generated for functions, data objects, and
  S4 classes and methods, using the \code{\link{prompt}},
  \code{\link{promptClass}},  and \code{\link{promptMethods}} functions.
  If an object from another package is intended to be imported and
  re-exported without changes, the \code{\link{promptImport}} function
  should be used after \code{package.skeleton}
  to generate a simple help file linking to the original one.

  The package sources are placed in subdirectory \code{name} of
  \code{path}.  If \code{code_files} is supplied, these files are
  copied; otherwise, objects will be dumped into individual source
  files.  The file names in \code{code_files} should have suffix
  \code{".R"} and be in the current working directory.

  The filenames created for source and documentation try to be valid for
  all OSes known to run \R.  Invalid characters are replaced by \samp{_},
  invalid names are preceded by \samp{zz}, names are converted to lower
  case (to avoid case collisions on case-insensitive file systems) and
  finally the converted names are made unique by
  \code{\link{make.unique}(sep = "_")}.  This can be done for code and
  help files but not data files (which are looked for by name). Also,
  the code and help files should have names starting with an ASCII
  letter or digit, and this is checked and if necessary \code{z}
  prepended.

  Functions with names starting with a dot are placed in file
  \file{R/\var{name}-internal.R}.

  When you are done, delete the \file{Read-and-delete-me} file, as it
  should not be distributed.
}

\references{
  Read the \sQuote{Writing R Extensions} manual for more details.

  Once you have created a \emph{source} package you need to install it:
  see the \sQuote{R Installation and Administration} manual,
  \code{\link{INSTALL}} and \code{\link{install.packages}}.
}

\seealso{
  \code{\link{prompt}}, \code{\link{promptClass}}, and
  \code{\link{promptMethods}}.

  \code{\link{package_native_routine_registration_skeleton}} for helping
  in preparing packages with compiled code.
}

\examples{
require(stats)
## two functions and two "data sets" :
f <- function(x, y) x+y
g <- function(x, y) x-y
d <- data.frame(a = 1, b = 2)
e <- rnorm(1000)
\dontshow{
  owd <- getwd()
  setwd(tempdir())
}
package.skeleton(list = c("f","g","d","e"), name = "mypkg")
\dontshow{ setwd(owd) }
}
\keyword{file}
\keyword{utilities}
