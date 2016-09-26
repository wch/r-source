% File src/library/methods/man/Methods_for_S3.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later
\name{Methods_for_S3}
\alias{Methods_for_S3}
\title{Methods For S3 and S4 Dispatch}

\description{

The S3 and S4 software in \R are two generations implementing
functional object-oriented programming.
S3 is the original, simpler for initial programming but less general,
less formal and less open to validation.
The S4 formal methods and classes provide these features but require
more programming.

In modern \R, the two versions attempt to work together.  This
documentation outlines how to write methods for both systems by
defining an S4 method for a function that dispatches S3 methods.

The systems can also be combined by using an S3 class with S4 method
dispatch or in S4 class definitions.  See \code{\link{setOldClass}}.

}
\section{S3 Method Dispatch}{

The \R evaluator will \sQuote{dispatch} a method from a function call
either when the body of the function calls the special primitive
\code{\link{UseMethod}} or when the call is to one of the builtin
primitives such as the \code{math} functions or the binary operators.


S3 method dispatch looks at the class of the first
argument or the class of either
argument in a call to one of the primitive binary operators.
In pure S3 situations, \sQuote{class} in this context means the class
attribute or the implied class for a basic data type such as
\code{"numeric"}.
The first S3 method that matches a name in the class is called and the
value of that call is the value of the original function call.
For details, see \link{S3Methods}.

In modern \R, a function \code{meth} in a package is registered as an S3 method
for function \code{fun} and class \code{Class} by
including in the package's \code{NAMESPACE} file the directive

\code{S3method(fun, Class, meth)}

By default (and traditionally), the third argument is taken to be the
function \code{fun.Class}; that is,
the name of the
generic function, followed by \code{"."}, followed by the name of the
class.

As with S4 methods, a method that has been registered will be added to
a table of methods for this function when the corresponding package is
loaded into the session.
Older versions of \R, copying the mechanism in S, looked for the
method in the current search list, but packages should now always
register S3 methods rather than requiring the package to be attached.
}

\section{Methods for S4 Classes}{

Two possible mechanisms for implementing a method  corresponding to an
S4 class, there are two possibilities are to register it as an S3 method with the
S4 class name or to define and set an S4 method, which will have the
side effect of creating an S4 generic version of this function.

For most situations either works, but
the recommended approach is to do both: register the S3 method and supply the
identical function as the definition of the S4 method.
This ensures that the proposed method will be dispatched for any
applicable call to the function.

As an example, suppose an S4 class \code{"uncased"} is defined,
extending \code{"character"} and intending to ignore upper- and
lower-case.
The base function \code{\link{unique}} dispatches S3 methods.
To define the class and a method for this function:

\code{setClass("uncased", contains = "character")}

\code{unique.uncased <- function(x, incomparables = FALSE, ...)
  nextMethod(tolower(x))}

\code{setMethod("unique", "uncased", unique.uncased)}

In addition, the \code{NAMESPACE} for the package should contain:

\code{S3method(unique, uncased)}

\code{exportMethods(unique)}

The result is to define identical S3 and S4 methods and ensure that all
calls to \code{unique} will dispatch that method when appropriate.
}

\section{Details}{

The reasons for defining both S3 and S4 methods are as follows:
\enumerate{
  \item An S4 method alone will not be seen if the S3 generic function
    is called directly. This will be the case, for example, if some
    function calls \code{unique()} from a package that does not make
    that function  an S4 generic.

    However, primitive functions and operators
    are exceptions:  The internal C code will look for S4 methods
    if and only if the object is an S4 object.  S4 method dispatch
    would be used to dispatch any binary operator calls where either
    of the operands was an S4 object, for example.

  \item An S3 method alone will not be called if there is \emph{any}
    eligible non-default S4 method.

    So if a package defined an S3
    method for \code{unique} for an S4 class but another package
    defined an S4 method for a superclass of that class, the
    superclass method would be chosen, probably not what was
    intended.
}

S4 and S3 method selection are designed to follow compatible rules of
inheritance, as far as possible.
S3 classes can be used for any S4 method selection, provided that the
S3 classes have been registered by a call to
\code{\link{setOldClass}}, with that call specifying the correct S3
inheritance pattern.
S4 classes can be used for any S3 method selection; when an S4 object
is detected, S3 method selection uses the contents of
\code{\link{extends}(class(x))} as the equivalent of the S3
inheritance (the inheritance is cached after the first call).


For the details of S4 and S3
dispatch see \link{Methods_Details} and \link{S3Methods}.

}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}
