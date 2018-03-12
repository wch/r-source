% File src/library/methods/man/validObject.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{validObject}
\alias{validObject}
\alias{getValidity}
\alias{setValidity}
\title{ Test the Validity of an Object }
\description{
  \code{validObject()} tests the validity of \code{object} related to
  its class definition; specifically, it checks that all slots
  specified in the class definition are present and that the object in
  the slot is from the required class or a subclass of that class.

  If the object is valid, \code{TRUE} is returned; otherwise, an error
  is generated, reporting all the validity failures encountered.
If argument \code{test} is
  \code{TRUE}, the errors are returned as a character vector rather
  than generating an error.
  

  When an object from a class is initialized, the default method for
  \code{\link{initialize}()} calls \code{validObject}.

A class definition may have a validity method, set by a call to
  the function \code{setValidity}, in the package or environment that
  defines the class (or via the \code{validity} argument to \code{\link{setClass}}).  The method
  should be a function of one object that returns \code{TRUE} or a character-string
  description of the non-validity.
  If such a method exists, it will be called from \code{validObject}
  and any strings from failure will be included in the result or the
  error message.
Any validity methods defined for superclasses (from the \code{contains=}
argument to \code{\link{setClass}}), will also be called.


}
\usage{
validObject(object, test = FALSE, complete = FALSE)

setValidity(Class, method, where = topenv(parent.frame()) )

getValidity(ClassDef)
}
\arguments{
  \item{object}{ any object, but not much will happen unless the
    object's class has a formal definition.}
  \item{test}{logical; if \code{TRUE} and validity fails, the
    function returns a vector of strings describing the problems.  If
    \code{test} is \code{FALSE} (the default) validity failure generates
    an error.}
  \item{complete}{logical; if \code{TRUE}, \code{validObject} is
      called recursively for each of the slots.  The default is \code{FALSE}.}
  \item{Class}{the name or class definition of the class whose validity
    method is to be set.}
  \item{ClassDef}{a class definition object, as from
    \code{\link{getClassDef}}.}
  \item{method}{a validity method;  that is, either \code{NULL} or a
    function of one argument (\code{object}).  Like
    \code{validObject}, the function should return \code{TRUE} if the
    object is valid, and one or more descriptive strings if any problems
    are found.  Unlike \code{validObject}, it should never generate an
    error.
  }
  \item{where}{an environment to store the modified class
      definition. Should be omitted, specifically  for calls from a package that defines the class.
    The definition will be stored in the
    namespace of the package.}

}
\section{Validity methods}{
 A validity method must be a function of one argument; formally, that
argument should be named \code{object}.
If the argument has a different name, \code{setValidity} makes the
substitution but in obscure cases that might fail, so it's wiser to
name the
 argument \code{object}.

A good method checks all the possible errors and returns a character
vector citing all the exceptions found, rather than returning after
the first one.
\code{validObject} will accumulate these errors in its error message
or its return value.

Note that validity methods do not have to check validity of
  superclasses: \code{validObject} calls such methods explicitly.
}
\details{
  Validity testing takes place \sQuote{bottom up}, checking the slots,
  then the superclasses, then the object's own validity method, if
  there is one.

For each slot and superclass, the existence of the specified class is
checked.
For each slot, the object in the slot is tested for inheritance from
the corresponding class.
If  \code{complete} is {TRUE},   \code{validObject} is called
recursively for the object in the slot.

Then, for each of the classes that this class
  extends (the \sQuote{superclasses}), the explicit validity method of
  that class is called, if one exists.  Finally, the validity method of
  \code{object}'s class is called, if there is one.

}
\value{
  \code{validObject} returns \code{TRUE} if the object is valid.
  Otherwise a vector of strings describing problems found, except that
  if \code{test} is \code{FALSE}, validity failure generates an error,
  with the corresponding strings in the error message.
}
\references{
Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}
\seealso{\code{\link{setClass}};
  class \code{\linkS4class{classRepresentation}}.
}
\examples{
setClass("track",
          slots = c(x="numeric", y = "numeric"))
t1 <- new("track", x=1:10, y=sort(stats::rnorm(10)))
## A valid "track" object has the same number of x, y values
validTrackObject <- function(object) {
    if(length(object@x) == length(object@y)) TRUE
    else paste("Unequal x,y lengths: ", length(object@x), ", ",
               length(object@y), sep="")
}
## assign the function as the validity method for the class
setValidity("track", validTrackObject)
## t1 should be a valid "track" object
validObject(t1)
## Now we do something bad
t2 <- t1
t2@x <- 1:20
## This should generate an error
\dontrun{try(validObject(t2))}
\dontshow{stopifnot(is(try(validObject(t2)), "try-error"))}

setClass("trackCurve", contains = "track",
         slots = c(smooth = "numeric"))

## all superclass validity methods are used when validObject
## is called from initialize() with arguments, so this fails
\dontrun{trynew("trackCurve", t2)}
\dontshow{stopifnot(is(try(new("trackCurve", t2)), "try-error"))}

setClass("twoTrack", slots = c(tr1 = "track", tr2 ="track"))

## validity tests are not applied recursively by default,
## so this object is created (invalidly)
tT  <- new("twoTrack", tr2 = t2)

## A stricter test detects the problem
\dontrun{try(validObject(tT, complete = TRUE))}
\dontshow{stopifnot(is(try(validObject(tT, complete = TRUE)), "try-error"))}
}
\keyword{programming}
\keyword{classes}
