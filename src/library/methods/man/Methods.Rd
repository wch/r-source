\name{Methods}
\alias{Methods}
\alias{plot}
\title{General Information on Methods}
\non_function{}
\description{

This documentation section covers some general topics on how methods work
and how the methods package interacts with the rest of R.
The information is usually not needed to get started with methods and
classes, but may be helpful for moderately ambitious projects, or when
something doesn't work as expected.

The section \bold{How Methods Work} 
describes the underlying mechanism; \bold{Class Inheritance and Method
  Selection} provides more details on how class definitions determine
which methods are used.

The section \bold{Changes with the Methods Package} outlines possible
effects on other computations when running with package
\code{methods}.

  
}
\section{How Methods Work}{

A generic function is a function that has
associated with it a
collection of other functions(the methods), all of which agree in
formal arguments with the generic.
In R, the ``collection'' is an object of class
\code{"\link{MethodsList}"}, which contains a named list of methods
(the \code{methods} slot),
and the name of one of the formal arguments to the function (the
\code{argument} slot).
The names of the methods are the names of classes, and the corresponding element
defines the method or methods to be used if the corresponding argument
has that class.
For example, suppose a function \code{f} has formal arguments
\code{x} and \code{y}.
The methods list object for that function has the object
\code{as.name("x")} as its \code{argument} slot.
An element of the methods named \code{"track"} is selected if the
actual argument corresponding to \code{x} is an object of class
\code{"track"}.
If there is such an element, it can generally be either a function or
another methods list object.

In the first case, the function defines the method to use for any call
in which \code{x} is of class
\code{"track"}.
In the second case, the new methods list object defines the selection
of methods depending on the remaining formal arguments,
in this example, \code{y}.
The same selection process takes place, recursively, using the new
methods list.
Eventually, the selection returns either a function or \code{NULL},
meaning that no method matched the actual arguments.

Each method selected corresponds conceptually to a \emph{signature};
that is a named list of classes, with names corresponding to some or
all of the formal arguments.
In the previous example, if selecting class \code{"track"} for
\code{x}, finding that the selection was another methods list and then
selecting class \code{"numeric"} for \code{y} would produce a method
associated with the signature \code{
   x = "track", y = "numeric"
}

The actual selection is done recursively, but you can see the methods
arranged by signature by calling the function
\code{\link{showMethods}}, and objects with the methods arranged
this way (in two different forms) are returned by the functions
\code{\link{listFromMlist}} and \code{\link{linearizeMlist}}.

In an R session, each generic function has a single methods list
object defining all the currently available methods.
The session methods list object is created the first time the function is called
by merging all the relevant method definitions currently visible.
Whenever something happens that
might change the definitions (such as attaching or detaching a package
with methods for this function, or explicitly defining or removing methods), the merged
methods list object is removed.  The next call to the function will
recompute the merged definitions.

When methods list are merged, they can come from two sources:
\enumerate{
\item {
Methods list objects for the same function on one or more currently
attached databases.
These are merged so that methods in a database earlier in the search
list override methods for the same function later in the search list.
A method overrides only another method for the same signature.  See
the comments on class \code{"ANY"} in the section on \bold{Inheritance}.
}
\item {Methods list objects corresponding to the
    generic function itself and to the group generic functions, if
    any, for this function.
    Any generic function can be defined to belong to a group generic.
    The methods for the group generic are available as methods for
    this function.  The group generic can itself be defined as belong
    to a group; as a result there is a list of group generic
    functions.  A method defined for a function and a particular
    signature overrides a method for the same signature for that
    function's group generic
}
}
Merging is done first over databases for a particular function, and
then over the generic and its group generics.

The result is a single methods list object that contains all the
methods \emph{directly} defined for this function.
As calls to the function occur, this information may be supplemented
by \emph{inherited} methods, which we consider next.
}
\section{Class Inheritance and Method Selection}{
If no method is found directly for the actual arguments in a call to a
generic function, an attempt is made to match the available methods to
the arguments by using \emph{inheritance}.

Each class definition potentially includes the names of one or more
classes that the new class extends.  (These are sometimes called the
\emph{superclasses} of the new class.)  These classes themselves may
extend other classes.  Putting all this information together produces
the full list of superclasses for this class. (You can see this list
for any class \code{"A"} from the expression \code{extends("A")}.)
In addition, any class implicitly extends class \code{"ANY"}.

A method will be selected by inheritance if we can find a method in
the methods list for a signature corresponding to any
combination of superclasses for each of the relevant arguments.
The search for such a method is performed by the function
\code{\link{MethodsListSelect}}, working as follows.

For the first formal argument of the function, a list of classes is
made up from the class itself, all its superclasses, and class
\code{"ANY"}.
For each of these, the selection computation looks for an element of
the methods with the corresponding name.  Each time it finds one, it
then calls the selection process recursively if necessary to select a
method directly or by inheritance for the remaining arguments.

Each one of these recursive calls can fail or it can return a function
(the method).  As long as the calls fail, the selection process moves
on to the next superclass and tries again.
The last step corresponds to class \code{"ANY"}, the default method
defined at this level.

The effect of this definition of the selection process is to order all
possible inherited methods, first by the superclasses for the first
argument, then within this by the superclasses for the second
argument, and so on.
Superclasses are ordered by how direct they are:  first, the direct
superclasses, then the superclasses of these classes.
}
\section{Changes with the Methods Package}{
The methods package is designed to leave other computations in R
unchanged.  There are, however, a few areas where the default
functions and behavior are overridden when running with the methods
package attached.
This section outlines those known to have some possible effect.

\describe{

\item{\code{class}:}{
    The methods package enforces the notion that every object has a
    class; in particular, \code{class(x)} is never \code{NULL}, as it
    would be for basic vectors, for example, when not using methods.

    In addition, when assigning a class, the value is required to be a
    single string.  (However, objects can have multiple class names if
    these were generated by old-style class computations.  The methods
    package does not hide the ``extra'' class names.)

    Computations using the notion of \code{NULL} class attributes or
    of class attributes with multiple class names are not really
    compatible with the ideas in the methods package.  Formal classes
    and class inheritance are designed to give more flexible and
    reliable implementations of similar ideas.

    If you do have to mix the two approaches, any operations that use
    class attributes in the old sense should be written in terms of
    \code{attr(x, "class")}, not \code{class(x)}.  In particular, test
    for no class having been assigned with \code{is.null(attr(x, "class"))}.
}

\item{Printing}{
    To provide appropriate printing   automatically for
    objects with formal class definitions, the methods package
    overrides \code{print.default}, to look for methods for the
    generic function \code{show}, and to use a default method for
    objects with formal class definitions.

    The revised version of \code{print.default} is intended to produce
    identical printing to the original version for any object that
    does \emph{not} have a formally defined class, including honoring
    old-style print methods.  So far, no exceptions are known.
  }

  \item{\code{plot}}{
    A version of the \code{plot} function is included in the current
    methods package, differing from the one in the base package in that
    it has a \code{y} argument (necessary if methods for plot are to be
    defined for the \code{y} data; see the examples for
    \link{setMethod}).
    This version will move into base as soon as it is tested. 
    }

}

}

\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\seealso{\code{\link{setGeneric}}, \code{\link{setClass}}}
\keyword{programming}
\keyword{classes}
\keyword{methods}
