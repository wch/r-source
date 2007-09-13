% File src/library/methods/man/StructureClasses.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{StructureClasses}
\title{Classes Corresponding to Basic Structures}
\docType{class}
%
\alias{structure-class}
\alias{matrix-class}
\alias{array-class}
\alias{ts-class}
% Group Methods of these
\alias{Math,structure-method}
\alias{Ops,structure,vector-method}
\alias{Ops,structure,structure-method}
\alias{Ops,structure,array-method}
\alias{Ops,vector,structure-method}
\alias{Ops,array,structure-method}
\alias{Ops,array,array-method}
%
\description{
  The virtual class \code{structure} and classes that
  extend it are formal classes analogous to S language structures such
  as arrays and time-series.
}
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
      example, \code{as(x, "matrix")} calls \code{as.matrix(x)}.}
    \item{Ops, Math}{Group methods (see, e.g., \code{\link{S4groupGeneric}})
      are defined for combinations of structures and vectors (including
      special cases for array and matrix), implementing the concept of
      vector structures as in the reference.
    }
  }
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
showClass("structure")

## explore a bit :
showClass("ts")
(ts0 <- new("ts"))
str(ts0)

showMethods("Ops") # six methods from these classes, but maybe many more
}
\keyword{classes}
