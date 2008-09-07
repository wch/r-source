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
\alias{initialize,array-method}
\alias{initialize,matrix-method}
\alias{initialize,ts-method}
\alias{initialize,mts-method}
\alias{show,ts-method}
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

  Objects created from the classes \code{"matrix"} and \code{"array"} are unusual, to put
  it mildly, and have been for some time.  Although they may appear to
  be objects from these classes, they do not have the internal
  structure of either an S3 or S4 class object.  In particular, they
  have no \code{"class"} attribute and are not recognized as objects
  with classes (that is, both \code{\link{is.object}} and
  \code{\link{isS4}} will return \code{FALSE} for such objects).

  That the objects still behave as if they came from the corresponding
  class (most of the time, anyway) results from special code
  recognizing such objects being built into the base code of \R.
  For most purposes, treating the classes in the usual way will work,
  fortunately.  One consequence of the special treatment is that these
  two classes\emph{may} be used as the data part of an S4 class; for
  example, you can get away with \code{contains = "matrix"} in a call
  to \code{\link{setGeneric}} to create an S4 class that is a subclass
  of \code{"matrix"}.  There is no guarantee that everything will work
  perfectly, but a number of classes have been written in this form
  successfully.

  The class \code{"ts"}  is basically an S3 class
  that has been registered with S4, using the
  \code{\link{setOldClass}} mechanism.  Versions of \R through 2.7.0
  treated this class as a pure S4 class, which was in principal a good
  idea, but in practice did not allow subclasses to be defined and had
  other intrinsic problems.  (For example, setting the
  \code{"tsp"} parameters as a slot often fails because the built-in
  implementation does not allow the slot to be temporarily
  inconsistent with the length of the data. Also, the S4 class
  prevented the correct specification of the S3 inheritance for class
  \code{"mts"}.)

  The current behavior (beginning with version 2.8.0 of \R) registers
  \code{"ts"} as an S3 class,  using an S4-style definition (see the
  documentation for \code{\link{setOldClass}} in the examples section
  for an abbreviated listing of how this is done.  The S3
  inheritance of \code{"mts"} in package \code{stats} is also
  registered.
  These classes, as well as \code{"matrix"} and \code{"array"} should
  be valid in most examples as superclasses for new S4 class
  definitions.


  
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
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole (for the original vector structures).
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
