\name{as}
\alias{as}
\alias{as<-}
\alias{coerce}
\alias{coerce<-}
\alias{setAs}
\alias{coerce-methods}
\alias{coerce,ANY,array-method}
\alias{coerce,ANY,call-method}
\alias{coerce,ANY,character-method}
\alias{coerce,ANY,complex-method}
\alias{coerce,ANY,environment-method}
\alias{coerce,ANY,expression-method}
\alias{coerce,ANY,function-method}
\alias{coerce,ANY,integer-method}
\alias{coerce,ANY,list-method}
\alias{coerce,ANY,logical-method}
\alias{coerce,ANY,matrix-method}
\alias{coerce,ANY,name-method}
\alias{coerce,ANY,numeric-method}
\alias{coerce,ANY,single-method}
\alias{coerce,ANY,ts-method}
\alias{coerce,ANY,vector-method}
\alias{coerce,ANY,NULL-method}
\title{Force an Object to Belong to a Class}
\description{
  These functions manage the relations that allow coercing an object to
  a given class.
}
\usage{
as(object, Class, strict=TRUE, ext)

as(object, Class) <- value

setAs(from, to, def, replace, where = topenv(parent.frame()))

}
\section{Summary of Functions}{
  \describe{
    \item{\code{as}:}{
      Returns the version of this object coerced to be the given
      \code{Class}.

      If the corresponding \code{is(object, Class)} relation is true, it
      will be used.
      In particular, if the relation has a coerce method, the method
      will be invoked on \code{object}.  However, if the object's
      class extends \code{Class} in a simple way (e.g, by including
      the superclass in the definition, then the actual coercion will
      be done only if \code{strict} is \code{TRUE} (non-strict
      coercion, is used in passing objects to methods).

      Coerce methods are pre-defined for basic classes (including all
      the types of vectors, functions and a few others). See
      \code{showMethods(coerce)} for a list of these.

      Beyond these two sources of methods, further methods are defined
      by calls to the \code{setAs} function.

    }

    \item{\code{coerce}:}{
      Coerce \code{from} to be of the same class as \code{to}.

      Not a function you should usually call explicitly.  The function
      \code{\link{setAs}} creates methods for \code{coerce} for the
      \code{as} function to use.
    }

    \item{\code{setAs}:}{
      The function supplied as the third argument is to be called to
      implement \code{as(x, to)} when \code{x} has class \code{from}.
      Need we add that the function should return a suitable object with
      class \code{to}.
    }
  }
}
\arguments{
  \item{object}{any \R object.}
  \item{Class}{the name of the class to which \code{object} should be
    coerced. }
  \item{strict}{logical flag.  If \code{TRUE}, the returned object
    must be strictly from the target class (unless that class is a
    virtual class, in which case the object will be from the closest
    actual class (often the original object, if that class extends the
    virtual class directly).

    If \code{strict = FALSE}, any simple extension of the target class
    will be returned, without further change.  A simple extension is,
    roughly, one that just adds slots to an existing class.}
  \item{value}{The value to use to modify \code{object} (see the
    discussion below).  You should supply an object with class
    \code{Class}; some coercion is done, but you're unwise to rely on
    it.}

  \item{from, to}{The classes between which \code{def} performs coercion.

    (In the case of the \code{coerce} function these are objects from
    the classes, not the names of the classes, but you're not expected
    to call \code{coerce} directly.)}
  \item{def}{function of one argument.  It will get an object from
    class \code{from} and had better return an object of class
    \code{to}. (If you want to save \code{setAs} a little work, make
    the name of the argument \code{from}, but don't worry about it,
    \code{setAs} will do the conversion.) }
  \item{replace}{if supplied, the function to use as a replacement
    method.}
  \item{where}{the position or environment in which to store the
    resulting method for \code{coerce}.}
  \item{ext}{the optional object
    defining how \code{Class} is extended by the class of the
    object (as returned by \code{\link{possibleExtends}}).
    This argument is used internally (to provide essential
    information for non-public classes), but you are unlikely to want
    to use it directly.
  }
  %% FIXME: ; by default, the ... environment.}
}

%% <FIXME>
%% Currently Rdconv cannot deal with markup in section titles.
%% \section{How Functions \code{as} and \code{setAs} Work}{
\section{How Functions `as' and `setAs' Work}{
  %% </FIXME>
  The function \code{as} contrives to turn \code{object} into an object
  with class \code{Class}.  In doing so, it uses information about
  classes and methods, but in a somewhat special way.  Keep in mind
  that objects from one class can turn into objects from another class
  either automatically or by an explicit call to the \code{as}
  function. Automatic conversion is special, and comes from the
  designer of one class of objects asserting that this class extends a
  another class  (see \code{\link{setClass}} and \code{\link{setIs}}).

  Because inheritance is a powerful assertion, it should be used
  sparingly (otherwise your computations may produce unexpected, and
  perhaps incorrect, results).  But objects can also be converted
  explicitly, by calling \code{as}, and that conversion is designed to
  use any inheritance information, as well as explicit methods.

  As a first step in conversion, the \code{as} function determines
  whether \code{is(object, Class)} is \code{TRUE}.  This can be the case
  either because the class definition of \code{object} includes
  \code{Class} as a \dQuote{super class} (directly or indirectly), or because
  a call to \code{\link{setIs}} established the relationship.

  Either way, the inheritance relation defines a method to coerce
  \code{object} to \code{Class}.  In the most common case, the method
  is just to extract from \code{object} the slots needed for
  \code{Class}, but it's also possible to specify a method explicitly in
  a \code{\link{setIs}} call.

  So, if inheritance applies, the \code{as} function calls the
  appropriate method.  If inheritance does not apply, and
  \code{coerceFlag} is \code{FALSE}, \code{NULL} is returned.

  By default, \code{coerceFlag} is \code{TRUE}.  In this case the
  \code{as} function goes on to look for a method for the function
  \code{coerce} for the signature \code{c(from = class(object), to =
    Class)}.

  Method selection is used in the \code{as} function in two special
  ways.
  First,  inheritance is
  applied for the argument \code{from} but not for the argument
  \code{to} (if you think about it, you'll probably agree that you
  wouldn't want the result to be from some class other than the
  \code{Class} specified).
  Second, the function tries to use inheritance information to convert
  the object indirectly, by first converting it to an inherited class.
  It does this by examining the classes that the \code{from} class
  extends, to see if any of them has an explicit conversion method.
  Suppose class \code{"by"} does:  Then the \code{as} function
  implicitly computes \code{as(as(object, "by"), Class)}.

  With this explanation as background, the function \code{setAs} does a
  fairly obvious computation:  It constructs and sets a method for the function
  \code{coerce} with signature \code{c(from, to)}, using the \code{def}
  argument to define the body of the method.  The function supplied as
  \code{def} can have one argument (interpreted as an object to be
  coerced) or two arguments (the \code{from} object and the \code{to}
  class).  Either way, \code{setAs} constructs a function of two
  arguments, with the second defaulting to the name of the \code{to}
  class.  The method will be called from \code{as} with the object
  as the only argument:  The default for the
  second argument is provided so the method can know the intended
  \code{to} class.

  The function \code{coerce}
  exists almost entirely as a repository for such methods, to be
  selected as described above by the \code{as} function.
  In fact, it
  would usually be a bad idea to call \code{coerce} directly, since then
  you would get inheritance on the \code{to} argument; as mentioned,
  this is not likely to be what you want.
}

%% <FIXME>
%% Currently Rdconv cannot deal with markup in section titles.
%% \section{The Function \code{as} Used in Replacements}{
\section{The Function `as' Used in Replacements}{
  %% </FIXME>
  When \code{as} appears on the left of an assignment, the intuitive
  meaning is \dQuote{Replace the part of \code{object} that was inherited from
  \code{Class} by the \code{value} on the right of the assignment.}

  This usually has a straightforward interpretation, but you can control
  explicitly what happens, and sometimes you should to avoid possible
  corruption of objects.

  When \code{object} inherits from \code{Class} in the usual way, by
  including the slots of \code{Class}, the default \code{as} method is
  to set the corresponding slots in \code{object} to those in
  \code{value}.

  The default computation may be reasonable, but usually only if all
  \emph{other} slots in \code{object} are unrelated to the slots being
  changed.  Often, however, this is not the case.  The class of
  \code{object} may have extended \code{Class} with a new slot whose
  value depends on the inherited slots.  In this case, you may want to
  define a method for replacing the inherited information that
  recomputes all the dependent information.  Or, you may just want to
  prohibit replacing the inherited information directly .

  The way to control such replacements is through the \code{replace}
  argument to function \code{setIs}.  This argument is a method that
  function \code{as} calls when used for replacement.  It can do
  whatever you like, including calling \code{stop} if you want to
  prohibit replacements.  It should return a modified object with the
  same class as the \code{object} argument to \code{as}.

  In R, you can also explicitly supply a replacement method, even in the
  case that inheritance does not apply, through the \code{replace}
  argument to \code{setAs}.  It works essentially the same way, but in
  this case by constructing a method for \code{"coerce<-"}.  (Replace
  methods for coercion without inheritance are not in the original
  description and so may not be compatible with S-Plus, at least not
  yet.)

  When inheritance does apply, coerce and replace methods can be
  specified through either \code{setIs} or \code{setAs}; the effect is
  essentially the same.
}

\section{Basic Coercion Methods}{
  Methods are pre-defined for coercing any object to one of the basic
  datatypes.  For example, \code{as(x, "numeric")} uses the existing
  \code{as.numeric} function.  These built-in methods can be listed by
  \code{showMethods("coerce")}.
}

\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes
  and methods in the book \emph{Programming with Data} (John
  M. Chambers, Springer, 1998), in particular sections 1.6, 2.7, 2.8,
  and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}
\examples{
## using the definition of class "track" from \link{Classes}

\dontshow{
setClass("track",
         representation(x="numeric", y="numeric"))
setClass("trackCurve",
         representation("track", smooth = "numeric"))
}

setAs("track", "numeric", function(from) from@y)

t1 <- new("track", x=1:20, y=(1:20)^2)

as(t1, "numeric")

## The next example shows:
##  1. A virtual class to define setAs for several classes at once.
##  2. as() using inherited information

setClass("ca", representation(a = "character", id = "numeric"))

setClass("cb", representation(b = "character", id = "numeric"))

setClass("id")
setIs("ca", "id")
setIs("cb", "id")


setAs("id", "numeric", function(from) from@id)

CA <- new("ca", a = "A", id = 1)
CB <- new("cb", b = "B", id = 2)

setAs("cb", "ca", function(from, to )new(to, a=from@b, id = from@id))

as(CB, "numeric")

\dontshow{
## should generate an error (should have been a function of one argument)
try(setAs("track", "numeric", function(x, y,z)x@y))
}
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
