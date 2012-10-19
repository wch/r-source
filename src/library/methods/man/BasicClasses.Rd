% File src/library/methods/man/BasicClasses.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{BasicClasses}
\docType{class}
\alias{character-class}
\alias{complex-class}
\alias{double-class}
\alias{expression-class}
\alias{externalptr-class}
\alias{integer-class}
\alias{list-class}
\alias{logical-class}
\alias{numeric-class}
\alias{raw-class}
\alias{single-class}
\alias{vector-class}
\alias{NULL-class}
\alias{builtin-class}
\alias{special-class}
\alias{function-class}
\alias{ANY-class}
\alias{VIRTUAL-class}
\alias{missing-class}
\alias{S4-class}
\alias{namedList-class}
\title{Classes Corresponding to Basic Data Types }
\description{
  Formal classes exist corresponding to the basic R object types, allowing
  these types to be used in method signatures, as slots in class
  definitions, and to be extended by new classes.}
\usage{
### The following are all basic vector classes.
### They can appear as class names in method signatures,
### in calls to as(), is(), and new().
"character"
"complex"
"double"
"expression"
"integer"
"list"
"logical"
"numeric"
"single"
"raw"

### the class
"vector"
### is a virtual class, extended by all the above

### the class
"S4"
### is an object type for S4 objects that do not extend
### any of the basic vector classes.  It is a virtual class.

### The following are additional basic classes
"NULL"     #  NULL objects
"function" #  function objects, including primitives
"externalptr" # raw external pointers for use in C code

"ANY"  # virtual classes used by the methods package itself
"VIRTUAL"
"missing"

"namedList" # the alternative to "list" that preserves
            # the names attribute
}
\section{Objects from the Classes}{
  If a class is not virtual (see section in \code{\link{Classes}}),
  objects can be created by calls of the form \code{new(Class, ...)},
  where \code{Class} is the quoted class name, and the remaining
  arguments if any are objects to be interpreted as vectors of this
  class.  Multiple arguments will be concatenated.

  The class \code{"expression"} is slightly odd, in that the \dots
  arguments will \emph{not} be evaluated; therefore, don't enclose them
  in a call to \code{quote()}.

  Note that class \code{"list"} is a pure vector.  Although lists with
  names go back to the earliest versions of S, they are an extension
  of the vector concept in that they have an attribute (which can now
  be a slot) and which is either \code{NULL} or a character vector of
  the same length as the vector.  If you want to guarantee that list
  names are preserved, use class \code{"namedList"}, rather than
  \code{"list"}.  Objects from this class must have a names attribute,
  corresponding to slot \code{"names"},
  of type \code{"character"}.  Internally, R treats names for
  lists  specially, which makes it impractical to have the corresponding slot in
  class \code{"namedList"} be a union of character names and \code{NULL}.

}

\section{Classes and Types}{
The basic classes include classes for the basic R types.  Note that
objects of these types will not usually be S4 objects
(\code{\link{isS4}} will return \code{FALSE}), although objects from
classes that contain the basic class will be S4 objects, still with
the same type.  The type as
returned by \code{\link{typeof}} will sometimes differ from the class,
either just from a choice of terminology (type \code{"symbol"} and
class \code{"name"}, for example) or because there is not a one-to-one
correspondence between class and type (most of the classes that
inherit from class \code{"language"} have type \code{"language"}, for example).
}


\section{Extends}{
  The vector classes extend \code{"vector"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{Methods are defined to coerce arbitrary objects to
      the vector classes, by calling the corresponding basic function, for
      example, \code{as(x, "numeric")} calls \code{as.numeric(x)}. }
  }
}
\keyword{classes}
