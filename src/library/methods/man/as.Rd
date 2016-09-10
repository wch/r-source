% File src/library/methods/man/as.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

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
      \code{Class}.  When used in the replacement form on the left of
      an assignment, the portion of the object corresponding to
      \code{Class} is replaced by \code{value}.

      The operation of \code{as()} in either form depends on the
      definition of coerce methods.  Methods are defined automatically
      when the two classes are related by inheritance; that is, when
      one of the classes is a subclass of the other.  See the section
      on inheritance  below for details.

      Coerce methods are also predefined for basic classes (including all
      the types of vectors, functions and a few others). See
      \code{showMethods(coerce)} for a list of these.

      Beyond these two sources of methods, further methods are defined
      by calls to the \code{setAs} function.

    }

    \item{\code{setAs}:}{
      Define methods for coercing an object of class \code{from} to be of class \code{to}; the \code{def} argument provides for direct coercing and the \code{replace} argument, if included, provides for replacement.  See the \dQuote{How} section below for details.
    }

    \item{\code{coerce}, \code{coerce<-}:}{
      Coerce \code{from} to be of the same class as \code{to}.

      These functions should not be called explicitly.  The function
      \code{\link{setAs}} creates methods for them for the
      \code{as} function to use.
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
    actual class, in particular the original object, if that class extends the
    virtual class directly).

    If \code{strict = FALSE}, any simple extension of the target class
    will be returned, without further change.  A simple extension is,
    roughly, one that just adds slots to an existing class.}
  \item{value}{The value to use to modify \code{object} (see the
    discussion below).  You should supply an object with class
    \code{Class}; some coercion is done, but you're unwise to rely on
    it.}

  \item{from, to}{The classes between which the coerce methods
      \code{def} and \code{replace} perform coercion.
    }
  \item{def}{function of one argument.  It will get an object from
    class \code{from} and had better return an object of class
    \code{to}. The convention is that
    the name of the argument is \code{from}; if another argument name
    is used, \code{setAs} will attempt to substitute \code{from}. }
  \item{replace}{if supplied, the function to use as a replacement
    method, when \code{as} is used on the left of an assignment.
    Should be a function of two arguments, \code{from, value},
    although \code{setAs} will attempt to substitute if the arguments differ.}
  \item{where}{the position or environment in which to store the
    resulting methods. For most applications, it is recommended to
    omit this argument and to include the call to \code{setAs} in source code that is evaluated at the top level; that is, either in an R session by something equivalent to a call to \code{\link{source}}, or as part of the R source code for a package.}
  \item{ext}{the optional object
    defining how \code{Class} is extended by the class of the
    object (as returned by \code{\link{possibleExtends}}).
    This argument is used internally (to provide essential
    information for non-public classes), but you are unlikely to want
    to use it directly.
  }
}

\section{Inheritance and Coercion}{

 Objects from one class can turn into objects from another class
  either automatically or by an explicit call to the \code{as}
  function.  Automatic conversion is special, and comes from the
  designer of one class of objects asserting that this class extends
  another class.  The most common case is that one or more class names
  are supplied in the \code{contains=} argument to \code{setClass}, in
  which case the new class extends each of the earlier classes (in the
  usual terminology, the earlier classes are \emph{superclasses} of
  the new class and it is a \emph{subclass} of each of them).

This form of inheritance is called \emph{simple} inheritance in \R.
See \code{\link{setClass}} for details.
Inheritance can also be defined explicitly by a call to
\code{\link{setIs}}.
The two versions have slightly different implications for coerce methods.
Simple inheritance implies that inherited slots behave identically in the subclass and the superclass.
Whenever two classes are related by simple inheritance, corresponding coerce methods
are defined for both direct and replacement use of \code{as}.
In the case of simple inheritance, these methods do the obvious
computation:  they extract or replace the slots in the object that
correspond to those in the superclass definition.

The implicitly defined coerce methods may be overridden by a call
to \code{setAs}; note, however, that the implicit methods are defined for each
subclass-superclass pair, so that you must override each of these
explicitly, not rely on inheritance.

When inheritance is defined by a call to \code{setIs}, the coerce methods are provided explicitly, not generated automatically.
Inheritance will apply (to the \code{from} argument, as described in  the section below).
You could also supply methods via \code{setAs} for non-inherited relationships, and now these also can be inherited.

For further on the distinction between simple and explicit inheritance, see \code{\link{setIs}}.

}
\section{How Functions 'as' and 'setAs' Work}{
  The function \code{as}  turns \code{object} into an object
  of class \code{Class}.  In doing so, it applies a \dQuote{coerce
    method}, using S4
  classes and methods, but in a somewhat special way.
Coerce methods are methods for the function \code{coerce} or, in the
replacement case the function \code{`coerce<-`}.
These functions have two arguments in method signatures, \code{from}
and \code{to}, corresponding to the class of the object and the
desired coerce class.
These functions must not be called directly, but are used to store
tables of methods for the use of \code{as}, directly and for
replacements.
In this section we will describe the direct case, but except where
noted the replacement case works the same way, using \code{`coerce<-`}
and the \code{replace} argument to \code{setAs}, rather than
\code{coerce} and the \code{def} argument.

Assuming the \code{object} is not already of the desired class,
\code{as} first looks for a method in the table of methods
 for the function
  \code{coerce} for the signature \code{c(from = class(object), to =
    Class)}, in the same way method selection would do its initial lookup.
To be precise, this means the table of both direct and inherited
methods, but inheritance is used specially in this case (see below).

If no method is found, \code{as} looks for one.
First, if either \code{Class} or \code{class(object)} is a superclass
of the other, the class definition will contain the information needed
to construct a coerce method.
In the usual case that the subclass contains the superclass (i.e., has
all its slots), the method is constructed either by extracting or
replacing the inherited slots.
Non-simple extensions (the result of a call to \code{\link{setIs}})
will usually contain explicit methods, though possibly not for replacement.

 If no subclass/superclass relationship provides a method, \code{as}
 looks for an inherited method, but applying, inheritance for the argument \code{from} only, not for
  the argument \code{to} (if you think about it, you'll probably agree
  that you wouldn't want the result to be from some class other than the
  \code{Class} specified). Thus,
  \code{selectMethod("coerce", sig, useInherited= c(from=TRUE, to= FALSE))}
  replicates the method selection used by \code{as()}.

In nearly all cases the method found in this way will be cached in the
table of coerce methods (the exception being subclass relationships with a test, which
are legal but discouraged).
So the detailed calculations should be done only on the first
occurrence of a coerce from \code{class(object)} to \code{Class}.

Note that  \code{coerce} is not a standard generic function.  It is
not intended to be called directly.  To prevent accidentally caching
an invalid inherited method, calls are routed to an equivalent call to
\code{as}, and a warning is issued.  Also, calls to
\code{\link{selectMethod}} for this function may not represent the
method that \code{as} will choose.  You can only trust the result if
the corresponding call to \code{as} has occurred previously in this
session.

  With this explanation as background, the function \code{setAs} does a
  fairly obvious computation:  It constructs and sets a method for the function
  \code{coerce} with signature \code{c(from, to)}, using the \code{def}
  argument to define the body of the method.  The function supplied as
  \code{def} can have one argument (interpreted as an object to be
  coerced) or two arguments (the \code{from} object and the \code{to}
  class).  Either way, \code{setAs} constructs a function of two
  arguments, with the second defaulting to the name of the \code{to}
  class.  The method will be called from \code{as} with the object
  as the \code{from} argument and no \code{to} argument, with the default for this argument being the name of the intended
  \code{to} class, so the method can use this information in messages.

The direct version of the \code{as} function also has a \code{strict=} argument that defaults to \code{TRUE}.
Calls during the evaluation of methods for other functions will set this argument to \code{FALSE}.
The distinction is relevant when the object being coerced is from a simple subclass of the \code{to} class; if \code{strict=FALSE} in this case, nothing need be done.
For most user-written coerce methods, when the two classes have no subclass/superclass, the \code{strict=} argument is irrelevant.

The \code{replace} argument to \code{setAs} provides a method for
\code{`coerce<-`}.
As with all replacement methods, the last argument of the method must
have the name \code{value} for the object on the right of the
assignment.
As with the \code{coerce} method, the first two arguments are
\code{from, to}; there is no \code{strict=} option for the replace case.

  The function \code{coerce} exists as a repository for
  such methods, to be selected as described above by the \code{as}
  function.  Actually dispatching the methods using
  \code{standardGeneric} could produce incorrect inherited methods, by using
   inheritance on the
  \code{to} argument; as mentioned, this is not the logic used for
  \code{as}.
  To prevent selecting and caching invalid methods, calls to
  \code{coerce} are
  currently mapped into calls to \code{as}, with a warning message.
}


\section{Basic Coercion Methods}{
  Methods are pre-defined for coercing any object to one of the basic
  datatypes.  For example, \code{as(x, "numeric")} uses the existing
  \code{as.numeric} function.  These built-in methods can be listed by
  \code{showMethods("coerce")}.
}

\seealso{
  If you think of using \code{try(as(x, cl))}, consider
  \code{\link{canCoerce}(x, cl)} instead.
}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\examples{
## using the definition of class "track" from \link{setClass}

\dontshow{
setClass("track", slots = c(x="numeric", y="numeric"))
setClass("trackCurve", contains = "track", slots = c(smooth = "numeric"))
}

setAs("track", "numeric", function(from) from@y)

t1 <- new("track", x=1:20, y=(1:20)^2)

as(t1, "numeric")

## The next example shows:
##  1. A virtual class to define setAs for several classes at once.
##  2. as() using inherited information

setClass("ca", slots = c(a = "character", id = "numeric"))

setClass("cb", slots = c(b = "character", id = "numeric"))

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
