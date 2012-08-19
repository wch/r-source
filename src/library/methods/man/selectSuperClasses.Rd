% File src/library/methods/man/selectSuperClass.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-12 R Core Team
% Distributed under GPL 2 or later

\name{selectSuperClasses}
\alias{selectSuperClasses}
\alias{.selectSuperClasses}
\title{Super Classes (of Specific Kinds) of a Class}
\description{
  Return superclasses of \code{ClassDef}, possibly only non-virtual or
  direct or simple ones.

  These functions are designed to be fast, and consequently only work
  with the \code{contains} slot of the corresponding class definitions.
}
\usage{
selectSuperClasses(Class, dropVirtual = FALSE, namesOnly = TRUE,
                   directOnly = TRUE, simpleOnly = directOnly,
                   where = topenv(parent.frame()))

.selectSuperClasses(ext, dropVirtual = FALSE, namesOnly = TRUE,
                    directOnly = TRUE, simpleOnly = directOnly)
}
\arguments{
  \item{Class}{name of the class or (more efficiently) the class
    definition object (see \code{\link{getClass}}).}
  \item{dropVirtual}{logical indicating if only non-virtual superclasses
    should be returned.}
  \item{namesOnly}{logical indicating if only a vector names instead of
    a named list class-extensions should be returned.}
  \item{directOnly}{logical indicating if only a \emph{direct} super
  classes should be returned.}
  \item{simpleOnly}{logical indicating if only simple class extensions
    should be returned.}
  \item{where}{(only used when \code{Class} is not a class definition)
    environment where the class definition of \code{Class} is found.}
  \item{ext}{for \code{.selectSuperClasses()} only, a \code{\link{list}}
    of class extensions, typically \code{\link{getClassDef}(..)@contains}.}
}
\note{The typical user level function is \code{selectSuperClasses()}
  which calls \code{.selectSuperClasses()}; i.e., the latter should only
  be used for efficiency reasons by experienced useRs.
}
\value{
  a \code{\link{character}} vector (if \code{namesOnly} is true, as per
  default) or a list of class extensions (as the \code{contains} slot in
  the result of \code{\link{getClass}}).
}
\seealso{
  \code{\link{is}}, \code{\link{getClass}}; further, the more technical
  class \code{\linkS4class{classRepresentation}} documentation.
}
\examples{
setClass("Root")
setClass("Base", contains = "Root", representation(length = "integer"))
setClass("A", contains = "Base", representation(x = "numeric"))
setClass("B", contains = "Base", representation(y = "character"))
setClass("C", contains = c("A", "B"))

extends("C")   #-->  "C"  "A" "B"  "Base" "Root"
selectSuperClasses("C") # "A" "B"
selectSuperClasses("C", direct=FALSE) # "A" "B"  "Base"  "Root"
selectSuperClasses("C", dropVirt = TRUE, direct=FALSE)# ditto w/o "Root"
}
\keyword{programming}
\keyword{classes}

