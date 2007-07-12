\name{detach}
\title{Detach Objects from the Search Path}
\usage{
detach(name, pos = 2, version, unload = FALSE)
}
\alias{detach}
\arguments{
  \item{name}{
    The object to detach.  Defaults to \code{search()[pos]}.
    This can be an unquoted name or a character string but \emph{not} a
    character vector.  If a number is supplied this is taken as \code{pos}.
  }
  \item{pos}{
    Index position in \code{\link{search}()} of database to
    detach.  When \code{name} is a number, \code{pos = name}
    is used.
  }
  \item{version}{A character string denoting a version number of the
    package to be removed.  This should be used only with a versioned
    installation of the package: see \code{\link{library}}.
  }
  \item{unload}{A logical value indicating whether or not to attempt to
    unload the namespace and S4 methods when a package is being
    detached.  If the package has a namespace and \code{unload} is
    \code{TRUE}, then \code{detach} will attempt to unload the
    namespace and remove any S4 methods defined by the package.  If
    the namespace is in use or \code{unload} is \code{FALSE}, no
    unloading will occur.
  }
}
\description{
  Detach a database, i.e., remove it from the \code{\link{search}()} path
  of available \R objects.  Usually, this is either a \code{\link{data.frame}}
  which has been \code{\link{attach}}ed or a package which was required
  previously.
}
\details{
  This most commonly used with a single number argument referring to a
  position on the search list, and can also be used with a unquoted or
  quoted name of an item on the search list such as \code{package:tools}.

  When a package have been loaded with an explicit version number it can
  be detached using the name shown by \code{\link{search}} or by
  supplying \code{name} and \code{version}: see the examples.

  If a package has a namespace, detaching it does not by default unload
  the namespace (and may not even with \code{unload=TRUE}), and
  detaching will not in general unload any dynamically loaded compiled
  code.  Further, registered S3 methods from the namespace will not be
  removed.  If you use \code{\link{library}} on a package whose name
  space is loaded, it attaches the exports of the loaded name space.
  So detaching and re-attaching a package may not refresh some
  or all components of the package, and is inadvisable.
}
\value{
  The attached database is returned invisibly, either as
  \code{\link{data.frame}} or as \code{\link{list}}.
}
\note{
  You cannot detach either the workspace (position 1) or the \pkg{base}
  package (the last item in the search list).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{attach}}, \code{\link{library}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{unloadNamespace}},
  \code{\link{library.dynam.unload}} .
}
\examples{
require(splines) # package
detach(package:splines)
## could equally well use detach("package:splines")
## but NOT  pkg <- "package:splines"; detach(pkg)
## Instead, use
library(splines)
pkg <- "package:splines"
detach(pos = match(pkg, search()))

## careful: do not do this unless 'splines' is not already loaded.
library(splines)
detach(2) # 'pos' used for 'name'

## an example of the name argument to attach
## and of detaching a database named by a character vector
attach_and_detach <- function(db, pos=2)
{
   name <- deparse(substitute(db))
   attach(db, pos=pos, name=name)
   print(search()[pos])
   eval(substitute(detach(n), list(n=name)))
}
attach_and_detach(women, pos=3)

\dontrun{## Using a versioned install
library(ash, version="1.0-9")  # or perhaps just library(ash)
# then one of
detach("package:ash", version="1.0-9")
# or
detach("package:ash_1.0-9")
}}
\keyword{data}
