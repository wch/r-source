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
\alias{function-class}
\alias{ANY-class}
\alias{VIRTUAL-class}
\alias{missing-class}
\title{Classes Corresponding to Basic Data Types }
\description{
  Formal classes exist corresponding to the basic R data types, allowing
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

### The following are additional basic classes
"NULL"     #  NULL objects
"function" #  function objects, including primitives
"externalptr" # raw external pointers for use in C code

"ANY"  # virtual classes used by the methods package itself
"VIRTUAL"
"missing"
}
\section{Objects from the Classes}{
  Objects can be created by calls of the form \code{new(Class, ...)},
  where \code{Class} is the quoted class name, and the remaining
  arguments if any are objects to be interpreted as vectors of this
  class.  Multiple arguments will be concatenated.

  The class \code{"expression"} is slightly odd, in that the \dots
  arguments will \emph{not} be evaluated; therefore, don't enclose them
  in a call to \code{quote()}.
}
\section{Extends}{
  Class \code{"vector"}, directly.
}
\section{Methods}{
  \describe{
    \item{coerce}{Methods are defined to coerce arbitrary objects to
      these classes, by calling the corresponding basic function, for
      example, \code{as(x, "numeric")} calls \code{as.numeric(x)}. }
  }
}
\keyword{classes}
