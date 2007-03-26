\name{StructureClasses}
\docType{class}
\alias{structure-class}
\alias{matrix-class}
\alias{array-class}
\alias{ts-class}
\title{Classes Corresponding to Basic Structures }
\description{  The virtual class \code{structure} and classes that
  extend it are formal classes analogous to S language structures such
  as arrays and time-series}
\usage{
## The following class names can appear in method signatures,
## as the class in as() and is() expressions, and, except for
## the classes commented as VIRTUAL, in calls to new()

"matrix"
"array"
"ts"

"structure" ## VIRTUAL
}
\section{Objects from the Classes}{
  Objects can be created by calls of the form \code{new(Class, ...)},
  where \code{Class} is the quoted name of the specific class (e.g.,
  \code{"matrix"}), and the other arguments, if any, are interpreted as
  arguments to the corresponding function, e.g., to function
  \code{matrix()}.  There is no particular advantage over calling those
  functions directly, unless you are writing software designed to work
  for multiple classes, perhaps with the class name and the arguments
  passed in.
}
\section{Extends}{
  The specific classes all extend class \code{"structure"}, directly, and
  class \code{"vector"}, by class \code{"structure"}.
}
\section{Methods}{
  \describe{
    \item{coerce}{Methods are defined to coerce arbitrary objects to
      these classes, by calling the corresponding basic function, for
      example, \code{as(x, "matrix")} calls \code{as.matrix(x)}. }
  }
}
\keyword{classes}
