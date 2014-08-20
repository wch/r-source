% File src/library/base/man/detach.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{detach}
\alias{detach}
\title{Detach Objects from the Search Path}
\usage{
detach(name, pos = 2L, unload = FALSE, character.only = FALSE,
       force = FALSE)
}
\arguments{
  \item{name}{
    The object to detach.  Defaults to \code{search()[pos]}.
    This can be an unquoted name or a character string but \emph{not} a
    character vector.  If a number is supplied this is taken as \code{pos}.
  }
  \item{pos}{
    Index position in \code{\link{search}()} of the database to
    detach.  When \code{name} is a number, \code{pos = name}
    is used.
  }
  \item{unload}{A logical value indicating whether or not to attempt to
    unload the namespace when a package is being detached.  If the
    package has a namespace and \code{unload} is \code{TRUE}, then
    \code{detach} will attempt to unload the namespace \emph{via}
    \code{\link{unloadNamespace}}: if the namespace is imported by
    another namespace or \code{unload} is \code{FALSE}, no unloading
    will occur.
  }
  \item{character.only}{a logical indicating whether \code{name}
    can be assumed to be a character string.}
  \item{force}{logical: should a package be detached even though other
    attached packages depend on it?}
}
\description{
  Detach a database, i.e., remove it from the \code{\link{search}()}
  path of available \R objects.  Usually this is either a
  \code{\link{data.frame}} which has been \code{\link{attach}}ed or a
  package which was attached by \code{\link{library}}.
}
\details{
  This is most commonly used with a single number argument referring to a
  position on the search list, and can also be used with a unquoted or
  quoted name of an item on the search list such as \code{package:tools}.

  If a package has a namespace, detaching it does not by default unload
  the namespace (and may not even with \code{unload = TRUE}), and
  detaching will not in general unload any dynamically loaded compiled
  code (DLLs).  Further, registered S3 methods from the namespace will
  not be removed.  If you use \code{\link{library}} on a package whose
  namespace is loaded, it attaches the exports of the already loaded
  namespace.  So detaching and re-attaching a package may not refresh
  some or all components of the package, and is inadvisable.
}
\section{Good practice}{
  \code{detach()} without an argument removes the first item on the
  search path after the workspace.  It is all too easy to call it too
  many or too few times, or to not notice that the search path has
  changed since an \code{\link{attach}} call.

  Use of \code{attach}/\code{detach} is best avoided in functions (see
  the help for \code{\link{attach}}) and in interactive use and scripts
  it is prudent to detach by name.
}
\note{
  You cannot detach either the workspace (position 1) nor the \pkg{base}
  package (the last item in the search list), and attempting to do so
  will throw an error.

  Unloading some namespaces has undesirable side effects:
  e.g.\sspace{}unloading \pkg{grid} closes all graphics devices, and on some
  systems \pkg{tcltk} cannot be reloaded once it has been unloaded and
  may crash \R if this is attempted.
}
\value{
  The return value is \link{invisible}.  It is \code{NULL} when a
  package is detached, otherwise the environment which was returned by
  \code{\link{attach}} when the object was attached (incorporating any
  changes since it was attached).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{attach}}, \code{\link{library}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{unloadNamespace}},
  \code{\link{library.dynam.unload}} .
}
\examples{
require(splines) # package
detach(package:splines)
## or also
library(splines)
pkg <- "package:splines"
\dontshow{
stopifnot(inherits(tryCatch(detach(pkg), error = function(.).),  "error"))
}
detach(pkg, character.only = TRUE)

## careful: do not do this unless 'splines' is not already attached.
library(splines)
detach(2) # 'pos' used for 'name'

## an example of the name argument to attach
## and of detaching a database named by a character vector
attach_and_detach <- function(db, pos = 2)
{
   name <- deparse(substitute(db))
   attach(db, pos = pos, name = name)
   print(search()[pos])
   detach(name, character.only = TRUE)
}
attach_and_detach(women, pos = 3)
}
\keyword{data}
