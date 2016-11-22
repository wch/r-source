% File src/library/methods/man/setIs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{setIs}
\alias{setIs}
\title{Specify a Superclass Explicitly}
\description{
  \code{setIs} is an explicit alternative
  to the \code{contains=} argument to \code{\link{setClass}}.  It is
  only needed to create relations with explicit test or coercion.
  These have not proved to be of much practical value, so this
  function should not likely be needed in applications.

  Where the programming goal is to define methods for transforming one
  class of objects to another, it is usually better practice to call
  \code{\link{setAs}()}, which requires the transformations to be done explicitly.
}

\usage{
setIs(class1, class2, test=NULL, coerce=NULL, replace=NULL,
      by = character(), where = topenv(parent.frame()), classDef =,
      extensionObject = NULL, doComplete = TRUE)
}
\arguments{
  \item{class1, class2}{
    the names of the classes between which \code{is} relations are to be
    examined defined, or (more efficiently) the class definition
    objects for the classes.}

  \item{coerce, replace}{
    functions optionally supplied to coerce the object to
    \code{class2}, and to alter the object so that \code{is(object, class2)}
    is identical to \code{value}.  See the details section below.}

  \item{test}{
    a \emph{conditional} relationship is
    defined by supplying this function.  Conditional relations are
    discouraged and are not included in selecting methods.  See the details section below.

  The remaining arguments are for internal use and/or usually omitted.}

  \item{extensionObject}{ alternative to the \code{test, coerce,
    replace, by} arguments; an object from class
    \code{SClassExtension} describing the relation.  (Used in internal calls.)}

  \item{doComplete}{when \code{TRUE}, the class definitions will be
    augmented with indirect relations as well.  (Used in internal calls.)}
  \item{by}{
    In a call to \code{setIs}, the name of an intermediary class.
    Coercion will proceed by first coercing to this class and from there
    to the target class.  (The intermediate coercions have to be valid.)}
  \item{where}{
    In a call to \code{setIs}, where to store the metadata defining the
    relationship.  Default is the global environment for calls from the
    top level of the session or a source file evaluated there.  When the
    call occurs in the top level of a file in the source of a package,
    the default will be the namespace or environment of the package.
    Other uses are tricky and not usually a good idea, unless you really
    know what you are doing.}
  \item{classDef}{
    Optional class definition for \code{class} , required internally
    when \code{setIs} is called during the initial definition of the
    class by a call to \code{\link{setClass}}. \emph{Don't} use this
    argument, unless you really know why you're doing so.}
}

\section{Details}{
  Arranging for a class to inherit from another class is a key tool in
  programming.  In \R, there are three basic techniques, the first two
  providing what is called  \dQuote{simple} inheritance, the preferred form:
  %%
  \enumerate{
  \item
    By the \code{contains=} argument in a call to \code{\link{setClass}}.  This
      is and should be the most common mechanism.  It arranges that the new
      class contains all the structure of the existing class, and in
      particular all the slots with the same class specified.  The
      resulting class extension is defined to be \code{simple}, with
      important implications for method definition (see the section on
      this topic below).

  \item
     Making \code{class1} a subclass of a virtual class
      either by a call to \code{\link{setClassUnion}} to make the
      subclass a member of a new class union, or by a call to
      \code{setIs} to add a class to an existing class union or as a new
      subclass of an existing virtual class.  In either case, the
      implication should be that methods defined for the class union or
      other superclass all work correctly for the subclass.  This may
      depend on some similarity in the structure of the subclasses or
      simply indicate that the superclass methods are defined in terms
      of generic functions that apply to all the subclasses.  These
      relationships are also generally simple.


  \item
    Supplying \code{coerce}  and \code{replace} arguments to \code{setAs}.
      \R allows arbitrary inheritance relationships, using the same
      mechanism for defining coerce methods by a call to
      \code{\link{setAs}}.  The difference between the  two is simply
      that \code{\link{setAs}} will require a call to \code{\link{as}}
      for a conversion to take place, whereas after the call to
      \code{\link{setIs}}, objects will be automatically converted to
      the superclass.

      The automatic feature is the dangerous part, mainly because it
      results in the subclass potentially inheriting methods that do
      not work.  See the section on inheritance below.  If the two
      classes involved do not actually inherit a large collection of
      methods, as in the first example below, the danger may be
      relatively slight.

      If the superclass inherits methods where the subclass has only a
      default or remotely inherited method, problems are more likely.
      In this case, a general
      recommendation is to use the \code{\link{setAs}} mechanism
      instead, unless there is a strong counter reason. Otherwise, be prepared to
      override some of  the methods inherited.
  }

  With this caution given, the rest of this section describes what
  happens when \code{coerce=} and \code{replace=} arguments are supplied
  to \code{setIs}.

  The \code{coerce} and \code{replace} arguments are functions that
  define how to coerce a \code{class1} object to \code{class2}, and
  how to replace the part of the subclass object that corresponds to
  \code{class2}.  The first of these is a function of one argument
  which should be \code{from}, and the second of two arguments
  (\code{from}, \code{value}).  For details, see the section on coerce
  functions below .

  When \code{by} is specified, the coerce process first coerces to
  this class and then to \code{class2}.  It's unlikely you
  would use the \code{by} argument directly, but it is used in defining
  cached information about classes.

  The value returned (invisibly) by
  \code{setIs} is the revised class definition of \code{class1}.
}

\section{Coerce, replace, and test functions}{

  The  \code{coerce} argument is a function that turns a
  \code{class1} object into a \code{class2} object.  The
  \code{replace} argument is a function of two arguments that modifies a \code{class1}
  object (the first argument) to replace the part of it that
  corresponds to \code{class2} (supplied as \code{value}, the second
  argument).  It then returns the modified object as the value of the
  call.  In other words, it acts as a replacement method to
  implement the expression \code{as(object, class2) <- value}.

  The easiest way to think of the  \code{coerce} and \code{replace}
  functions is by thinking of the case that  \code{class1}
  contains \code{class2} in the usual sense, by including the slots of
  the second class.  (To repeat, in this situation you would not call
  \code{setIs}, but the analogy shows what happens when you do.)

  The \code{coerce} function in this case would just make a
  \code{class2} object by extracting the corresponding slots from the
  \code{class1} object. The \code{replace} function would replace in
  the \code{class1} object the slots corresponding to \code{class2},
  and return the modified object as its value.

  For additional discussion of these functions, see
  the documentation of the
   \code{\link{setAs}} function.  (Unfortunately, argument
   \code{def} to that function corresponds to argument \code{coerce} here.)

  The inheritance relationship can also be conditional, if a function is supplied as the
  \code{test} argument.  This should be a function of one argument
  that returns \code{TRUE} or \code{FALSE} according to whether the
  object supplied satisfies the relation \code{is(object, class2)}.
  Conditional relations between
  classes are discouraged in general because they require a per-object
  calculation to determine their validity. They cannot be applied
  as efficiently as ordinary relations and tend to make the code that
  uses them harder to interpret.  \emph{NOTE:  conditional inheritance
    is not used to dispatch methods.}  Methods for conditional
  superclasses will not be inherited.  Instead, a method for the
  subclass should be defined that tests the conditional relationship.
}
\section{Inherited methods}{
  A method written for a particular signature (classes matched to one
  or more formal arguments to the function) naturally assumes that the
  objects corresponding to the arguments can be treated as coming from
  the corresponding classes.  The objects will have all the slots and
  available methods for the classes.

  The code that selects and dispatches the methods ensures that this
  assumption is correct.  If the inheritance was \dQuote{simple}, that
  is, defined by one or more uses of the \code{contains=} argument in
  a call to \code{\link{setClass}}, no extra work is generally
  needed.  Classes are inherited from the superclass, with the same
  definition.

  When inheritance is defined by a general call to
  \code{setIs}, extra computations are required.  This form of
  inheritance implies that the subclass does \emph{not} just contain
  the slots of the superclass, but instead requires the explicit call
  to the coerce and/or replace method.  To ensure correct computation,
  the inherited method is supplemented by calls to \code{\link{as}}
  before the body of the method is evaluated.

  The calls to \code{\link{as}} generated in this case have the
  argument \code{strict = FALSE}, meaning that extra information can
  be left in the converted object, so long as it has all the
  appropriate slots.  (It's this option that allows simple subclass
  objects to be used without any change.)  When you are writing your
  coerce method, you may want to take advantage of that option.

  Methods inherited through non-simple extensions can result in ambiguities
  or unexpected selections.  If \code{class2} is a specialized class
  with just a few applicable methods, creating the inheritance
  relation may have little effect on the behavior of \code{class1}.
  But if \code{class2} is a class with many methods, you may
  find that you now inherit some undesirable methods for
  \code{class1}, in some cases, fail to inherit expected methods.
  In the second example below, the non-simple inheritance from class
  \code{"factor"} might be assumed to inherit S3 methods via that
  class.  But the S3 class is ambiguous, and in fact is
  \code{"character"} rather than \code{"factor"}.

  For some generic functions, methods inherited by non-simple
  extensions are either known to be invalid or sufficiently likely to
  be so that the generic function has been defined to exclude such
  inheritance.  For example \code{\link{initialize}} methods must
  return an object of the target class; this is straightforward if the
  extension is simple, because no change is made to the argument
  object, but is essentially impossible.  For this reason, the generic
  function insists on only simple extensions for inheritance.  See the
  \code{simpleInheritanceOnly} argument to \code{\link{setGeneric}}
  for the mechanism.  You can use this mechanism when defining new
  generic functions.

  If you get into problems with functions that do allow non-simple
  inheritance, there are two basic choices.  Either
  back off from the \code{setIs} call and settle for explicit coercing
  defined by a call to \code{\link{setAs}}; or, define explicit
  methods involving \code{class1} to override the bad inherited
  methods.  The first choice is the safer, when there are serious
  problems.

}


\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}

\examples{
\dontshow{
## A simple class with two slots
setClass("track",
         slots = c(x="numeric", y="numeric"))
## A class extending the previous, adding one more slot
}
## Two examples of setIs() with coerce= and replace= arguments
## The first one works fairly well, because neither class has many
## inherited methods do be disturbed by the new inheritance

## The second example does NOT work well, because the new superclass,
## "factor", causes methods to be inherited that should not be.

## First example:
## a class definition (see \link{setClass} for class "track")
setClass("trackCurve", contains = "track",
         slots = c( smooth = "numeric"))
## A class similar to "trackCurve", but with different structure
## allowing matrices for the "y" and "smooth" slots
setClass("trackMultiCurve",
         slots = c(x="numeric", y="matrix", smooth="matrix"),
         prototype = structure(list(), x=numeric(), y=matrix(0,0,0),

                               smooth= matrix(0,0,0)))
## Automatically convert an object from class "trackCurve" into
## "trackMultiCurve", by making the y, smooth slots into 1-column matrices
setIs("trackCurve",
      "trackMultiCurve",
      coerce = function(obj) {
        new("trackMultiCurve",
            x = obj@x,
            y = as.matrix(obj@y),
            smooth = as.matrix(obj@smooth))
      },
      replace = function(obj, value) {
        obj@y <- as.matrix(value@y)
        obj@x <- value@x
        obj@smooth <- as.matrix(value@smooth)
        obj})


\dontshow{
removeClass("trackMultiCurve")
removeClass("trackCurve")
removeClass("track")
}

## Second Example:
## A class that adds a slot to "character"
setClass("stringsDated", contains = "character",
         slots = c(stamp="POSIXt"))

## Convert automatically to a factor by explicit coerce
setIs("stringsDated", "factor",
      coerce = function(from) factor(from@.Data),
      replace= function(from, value) {
                  from@.Data <- as.character(value); from })
\dontshow{
set.seed(750)
}
ll <- sample(letters, 10, replace = TRUE)
ld <- new("stringsDated", ll, stamp = Sys.time())

levels(as(ld, "factor"))
levels(ld) # will be NULL--see comment in section on inheritance above.

## In contrast, a class that simply extends "factor"
## has no such ambiguities
setClass("factorDated", contains = "factor",
         slots = c(stamp="POSIXt"))
fd <- new("factorDated", factor(ll), stamp = Sys.time())
identical(levels(fd), levels(as(fd, "factor")))
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
