% File src/library/methods/man/setAs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{setAs}
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
\title{Methods for Coercing an Object to a Class}
\description{
  A call to \code{setAs} defines a method for coercing an object of
  class \code{from} to class \code{to}.  The methods will then be used
  by calls to \code{\link{as}} for objects with class \code{from},
  including calls that replace part of the object.

  Methods for this purpose work indirectly, by defining methods for
  function \code{coerce}.  This function is \emph{not} to be called
  directly, and method selection uses class inheritance only on the
  first argument.
}
\usage{

setAs(from, to, def, replace, where = topenv(parent.frame()))

}

\arguments{

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
    although \code{setAs} will attempt to substitute if the arguments
    differ.

\emph{The remaining arguments will not be used in standard applications.}

}

  \item{where}{the position or environment in which to store the
    resulting methods. Do not use this argument when defining a method
    in a package.  Only the default, the namespace of the package,
    should be used in normal situations.
    }
  \item{ext}{the optional object
    defining how \code{Class} is extended by the class of the
    object (as returned by \code{\link{possibleExtends}}).
    This argument is used internally (to provide essential
    information for non-public classes).
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
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
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
