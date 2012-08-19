% File src/library/methods/man/className.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2011-2 R Core Team
% Distributed under GPL 2 or later

\name{className}
\alias{className}
\alias{multipleClasses}
\alias{className-class}
\title{
Class names including the corresponding package
}
\description{
The function \code{className()} generates a
valid references to a class, including the name of the package
containing the class definition.  The object returned, from class \code{"className"}, is the
unambiguous way to refer to a class, for example when calling
\code{\link{setMethod}}, just in case multiple definitions of the
class exist.

Function
\code{"multipleClasses"} returns information about multiple
definitions of classes with the
same name from different packages.
}
\usage{

className(class, package)

multipleClasses(details = FALSE)
}
\arguments{

  \item{class, package}{
The character string name of a class and, optionally, of the package
to which it belongs.  If argument \code{package} is missing and the
\code{class} argument has a package slot, that is used  (in
particular, passing in an object from class \code{"className"} returns
itself in this case, but changes the package slot if the second
argument is supplied).

If there is no package argument or slot, a
definition for the class must exist and will be used to define the
package.  If there are multiple definitions, one will be chosen and a
warning printed giving the other possibilities.
}

  \item{details}{
If \code{FALSE}, the default, \code{multipleClasses()} returns a
character vector of those classes currently known with multiple
definitions.

If \code{TRUE}, a named list of those class definitions is returned.
Each element of the list is itself a list of the corresponding class
definitions, with the package names as the names of the list.  Note
that identical class definitions will not be considered
\dQuote{multiple} definitions (see the discussion of the details below).
}
}
\details{
The table of class definitions used internally can maintain multiple
definitions for classes with the same name but coming from different
packages.
If identical class definitions are encountered, only one class
definition is kept; this occurs most often with S3 classes that have
been specified in calls to \code{\link{setOldClass}}.  For true
classes, multiple class definitions are unavoidable in general if two
packages happen to have used the same name, independently.

Overriding a class definition in another package with the same name deliberately is usually a bad
idea.
Although \R attempts to keep and use the two definitions (as of
version 2.14.0), ambiguities are always possible.  It is more
sensible to define a new class that extends an existing class but has
a different name.
}
\value{
A call to \code{className()} returns an object from class
\code{"className"}.

A call to \code{multipleClasses()} returns either a character
vector or a named list of class definitions.  In either case, testing
the length of the returned value for being greater than \code{0} is a
check for the existence of multiply defined classes.
}

\section{Objects from the Class}{
The class \code{"className"} extends \code{"character"} and has a slot
\code{"package"}, also of class \code{"character"}.
}

\examples{
\dontrun{
className("vector") # will be found, from package "methods"
className("vector", "magic") # OK, even though the class doesn't exist


className("An unknown class") # Will cause an error
}
}
\keyword{ classes }
\keyword{ programming }
