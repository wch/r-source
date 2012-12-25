% File src/library/utils/man/getAnywhere.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{getAnywhere}
\alias{getAnywhere}
\alias{argsAnywhere}
\alias{[.getAnywhere}
\alias{print.getAnywhere}
\title{Retrieve an R Object, Including from a Namespace}
\description{
  These functions locate all objects with name matching their argument,
  whether visible on the search path, registered as an S3 method or in a
  namespace but not exported.  \code{getAnywhere()} returns the objects
  and \code{argsAnywhere()} returns the arguments of any objects that
  are functions.
}
\usage{
getAnywhere(x)
argsAnywhere(x)
}
\arguments{
  \item{x}{a character string or name.}
}
\details{
  These functions look at all loaded namespaces, whether or not they are
  associated with a package on the search list.

  They do not search literally \dQuote{anywhere}: for example, local
  evaluation frames and namespaces that are not loaded will not be
  searched.

  Where functions are found as registered S3 methods, an attempt is
  made to find which namespace registered them.  This may not be
  correct, especially if namespaces have been unloaded.
}
\value{
  For \code{getAnywhere()} an object of class \code{"getAnywhere"}.
  This is a list with components
  \item{name}{the name searched for}
  \item{objs}{a list of objects found}
  \item{where}{a character vector explaining where the object(s) were found}
  \item{visible}{logical: is the object visible}
  \item{dups}{logical: is the object identical to one earlier in the
    list.}
  In computing whether objects are identical, their environments are ignored.

  Normally the structure will be hidden by the \code{print} method.
  There is a \code{[} method to extract one or more of the objects
  found.

  For \code{argsAnywhere()} one or more argument lists as returned by
  \code{\link{args}}.
}
\seealso{
  \code{\link{getS3method}} to find the method which would be used: this
  might not be the one of those returned by \code{getAnywhere} since it
  might have come from a namespace which was unloaded or be registered
  under another name.

  \code{\link{get}},  \code{\link{getFromNamespace}}, \code{\link{args}}
}
\examples{
getAnywhere("format.dist")
getAnywhere("simpleLoess") # not exported from stats
argsAnywhere(format.dist)
}
\keyword{data}
