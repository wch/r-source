% File src/library/methods/man/getClass.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{getClass}
\alias{getClass}
\alias{getClassDef}
\title{Get Class Definition }
\description{
  Get the definition of a class.
}
\usage{
getClass   (Class, .Force = FALSE, where,
            resolve.msg = getOption("getClass.msg", default=TRUE))
getClassDef(Class, where, package, inherits = TRUE,
            resolve.msg = getOption("getClass.msg", default=TRUE))
}
\arguments{
  \item{Class}{ the character-string name of the class, often with a
      \code{"package"} attribute as noted below under \code{package}.}
  \item{.Force}{ if \code{TRUE}, return \code{NULL} if the class is
    undefined; otherwise, an undefined class results in an error.}
  \item{where}{ environment from which to begin the search for the definition; by default,
    start at the top-level (global) environment and proceed through
    the search list.}
  \item{package}{ the name of the package asserted to hold the
      definition.  If it is a non-empty string it is used instead of
      \code{where}, as the first place to look for the class.
      Note that the package must be loaded but need not be attached.  By
      default, the package attribute of the \code{Class} argument is
      used, if any.  There will usually be a package attribute if
      \code{Class} comes from \code{class(x)} for some object.
    }
  \item{inherits}{logical; should the class definition be retrieved from
    any enclosing environment and also from the cache?  If \code{FALSE}
    only a definition in the environment \code{where} will be returned.}
  \item{resolve.msg}{\code{\link{logical}} indicating if \R should
    \code{\link{message}()} its decision if \code{Class} is found in
    multiple namespaces and one is chosen.}
}
\details{
  Class definitions are stored in metadata objects in a package
  namespace or other environment where they are defined.  When
  packages are loaded, the class definitions in the package are cached in an internal
  table.  Therefore, most calls to \code{getClassDef} will find the
  class in the cache or fail to find it at all, unless \code{inherits}
  is \code{FALSE}, in which case only the environment(s) defined by
  \code{package} or \code{where} are searched.

  The class cache allows for multiple definitions of the
  same class name in separate environments, with of course the
  limitation that the package attribute or package name must be
  provided in the call to

}
\value{
  The object defining the class. If the class definition is not found,
  \code{getClassDef} returns \code{NULL}, while \code{getClass}, which
  calls \code{getClassDef}, either generates an error or, if
  \code{.Force} is \code{TRUE}, returns a simple definition for the
  class.  The latter case is used internally, but is not typically
  sensible in user code.

  The non-null returned value is an object of class
  \code{\linkS4class{classRepresentation}}.  For all reasonable
  purposes, use this object only to extract information, rather than trying
  to modify it: Use functions such as \code{\link{setClass}} and
  \code{\link{setIs}} to create or modify class definitions.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{
  \link{Classes},
  \code{\link{setClass}},
  \code{\link{isClass}}.
}
\examples{
getClass("numeric") ## a built in class

cld <- getClass("thisIsAnUndefinedClass", .Force = TRUE)
cld ## a NULL prototype
## If you are really curious:
utils::str(cld)
## Whereas these generate errors:
try(getClass("thisIsAnUndefinedClass"))
try(getClassDef("thisIsAnUndefinedClass"))
}
\keyword{programming}
\keyword{classes}
