% File src/library/methods/man/is.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{is}
\alias{is}
\alias{extends}
\title{Is an Object from a Class?}
\description{
  Functions to test inheritance relationships between an object and a
  class or between two classes (\code{extends}).
}

\usage{
is(object, class2)

extends(class1, class2, maybe = TRUE, fullInfo = FALSE)
}
\arguments{
  \item{object}{any \R object.}
  \item{class1, class2}{
    the names of the classes between which \code{is} relations are to be
    examined defined, or (more efficiently) the class definition
    objects for the classes.}

  \item{fullInfo}{
    In a call to \code{extends}, with \code{class2} missing,
    \code{fullInfo} is a flag, which if \code{TRUE} causes a list of
    objects of class \code{\linkS4class{SClassExtension}} to be returned, rather than
    just the names of the classes.  Only the distance slot is likely to
    be useful in practice; see the \sQuote{Selecting Superclasses} section;

  }
  \item{maybe}{
    What to return for conditional inheritance.  But such
    relationships are rarely used and not recommended, so this
    argument should not be needed.
  }
}

\section{Selecting Superclasses}{

  A call to  \code{\link{selectSuperClasses}(cl)} returns a list of
  superclasses, similarly to
  \code{extends(cl)}.  Additional arguments restrict the class names
  returned to direct superclasses and/or to non-virtual classes.


  To find superclasses with more generally defined properties, one can program
  with the result returned by \code{extends} when called with one
  class as argument.
  By default, the call returns a character vector including the name of the class
  itself and of all its superclasses.
  Alternatively,
  if \code{extends} is called with \code{fullInfo =
    TRUE}, the return value is a named list, its names being the previous
  character vector.  The elements of the list corresponding to
  superclasses are objects of class
  \code{\linkS4class{SClassExtension}}. Of the information in these objects, one piece can be useful:
  the number of generations between the classes, given by the
  \code{"distance"} slot.

  Programming with the result of the call to \code{extends}, particularly using
  \code{\link{sapply}}, can select superclasses.
  The programming technique is to define a test function that returns
  \code{TRUE} for superclasses or relationships obeying some
  requirement. For example, to find only next-to-direct superclasses,
  use this function with the list of extension objects:

  \code{function(what) is(what, "SClassExtension") && what@distance == 2}

  or, to find only superclasses from \code{"myPkg"}, use this function
  with the simple vector of names:

  \code{function(what) getClassDef(what)@package == "myPkg"}

  Giving such functions as an argument to \code{\link{sapply}} called on the output of
  \code{extends} allows you to find
  superclasses with desired properties.  See the examples below.

  Note that the function using extension objects must test the class of its argument since,
  unfortunately for this purpose, the list returned by \code{extends} includes
  \code{class1} itself, as the object \code{TRUE}.
}


\seealso{
  Although \code{\link{inherits}} is defined for S3 classes, it has
  been modified so that the result returned is nearly always equivalent to
  \code{is}, both for S4 and non-S4 objects. Since it is implemented
  in C, it is somewhat faster.
  The only non-equivalences arise from use of \code{\link{setIs}},
  which should rarely be encountered.


 }

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\examples{
\dontrun{
## this example can be run if package XRPython from CRAN is installed.
supers <- extends("PythonInterface")
## find all the superclasses from package XR
fromXR <- sapply(supers,
    function(what) getClassDef(what)@package == "XR")
## print them
supers[fromXR]

## find all the superclasses at distance 2
superRelations <- extends("PythonInterface", fullInfo = TRUE)
dist2 <- sapply(superRelations,
    function(what) is(what, "SClassExtension") && what@distance == 2)
## print them
names(superRelations)[dist2]

}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
