\name{Methods}
\alias{Methods}
\alias{isGeneric}
\alias{isGroup}
\alias{removeGeneric}
\alias{standardGeneric}
\alias{getGenerics}
\alias{allGenerics}
\alias{dumpMethod}
\alias{existsFunction}
\alias{findFunction}
\alias{dumpMethods}
\alias{showMethods}
\alias{removeMethods}
\alias{signature}
\alias{setReplaceMethod}
\title{Methods and Generic Functions}
\usage{
isGeneric(f, where, fdef, getName = FALSE)

isGroup(f, where, fdef)

removeGeneric(f, where)

standardGeneric(f)

dumpMethod(f, signature, file, where, def)

existsFunction(f, generic = TRUE, where)

findFunction(f, generic=TRUE)

dumpMethods(f, file, signature, methods, where)

signature(...)

showMethods(f, where, classes, includeDefs=FALSE, inherited = TRUE)

removeMethods(f, where)

setReplaceMethod(f, ...)

getGenerics(where)
allGenerics(where)

}
\description{

This documentation describes how method definition and selection works,
and the functions that deal with formally defined methods.
Brief documentation of the functions is provided below.

The section \bold{How Methods Work} may help more than the individual
function documentation; it
describes the underlying mechanism.
  
\describe{
  \item{\code{isGeneric}:}{ 
    Is there a function named \code{f}, and if so, is it a generic?

    The \code{getName} argument allows a function to find the name from
    a function definition.  If it is \code{TRUE} then the name of the
    generic is returned, or \code{FALSE} if this is not a generic
    function definition.

    The behavior of \code{isGeneric} and \code{\link{getGeneric}} for
    primitive functions is slightly different.  These functions don't
    exist as formal function objects (for efficiency and historical
    reasons), regardless of whether methods have been defined for them.
    A call to \code{isGeneric} tells you whether methods have been
    defined for this primitive function, either on database \code{where}
    or anywhere in the current search list.  In contrast, a call to
    \code{\link{getGeneric}} will return what the generic for that
    function would be, even if no methods have been currently defined
    for it.
  
  }
  
  \item{\code{removeGeneric}:}{ 
  Remove the generic function of this name.  
  }
  \item{\code{standardGeneric}:}{ 
  Dispatches a method from the current function call for the generic function \code{fname}.  
  
  }
  \item{\code{getMethods}:}{ 
  The list of methods for the specified generic.  
  
  }

  \item{\code{dumpMethod}:}{ 
  Dump the method for this generic function and signature.  
  
  }
  \item{\code{existsFunction}:}{ 
  Is there a function of this name. If \code{generic==FALSE}, generic functions are not counted.

  
  }
  \item{\code{findFunction}:}{ 
  return all the elements of the search list on which a function
  definition for \code{name} exists.

  \emph{NOTE:} Use this rather than \code{\link{find}} with
  \code{mode="function"}, which is not as meaningful, and has a few subtle bugs from its use of
  regular expressions.
  
  }
  \item{\code{selectMethod}:}{ 
  Returns the method (a function) that R would use to evaluate a call to this generic,
with arguments corresponding to the specified signature.

f = the name of the generic function
sig = the signature of classes to match to the arguments of f.  
  
  }
  \item{\code{dumpMethods}:}{ 
  Dump all the methods for this generic.  
  }
  \item{\code{signature}:}{ 
  Returns a named list of classes to be matched to arguments of a
  generic function.
 
  }
  \item{\code{showMethods}:}{ 
  Show all the methods for the specified function.

If \code{where} is supplied, the definition from that database will be used; otherwise,
the current definition is used (which will include inherited methods that have arisen so
far in the session).

The argument \code{classes} is included for consistency with S-Plus, but currently ignored.
Similarly, omitting \code{f} is not currently supported.  
  
  }
  \item{\code{removeMethods}:}{ 
  removes all the methods defined for this generic function.

  }
  \item{\code{setReplaceMethod}:}{
  defines a method for \code{f}, used as a replacement function on the
  left of an assignment, by calling \code{\link{setMethod}} for the
  corresponding generic replacement function.  The \code{...} arguments
  are passed to \code{\link{setMethod}}.
}
\item{\code{getGenerics}, \code{allGenerics}}{
  \code{getGenerics} returns the names of all generic functions for
  which methods are defined on package(s) \code{where}; by default, on any
  of the packages in the current search list.

  \code{allGenerics} is the identical function, supplied for S-Plus compatibility.
  }
}
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
\item{
Methods list objects for the same function on one or more currently
attached databases.
These are merged so that methods in a database earlier in the search
list override methods for the same function later in the search list.
A method overrides only another method for the same signature.  See
the comments on class \code{"ANY"} in the section on \bold{Inheritance}.
}
\item{Methods list objects corresponding to the
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
\arguments{
  \item{f}{ The character string naming the function. }
  \item{where}{ Where on the search list of attached packages to look
    for functions or methods.  By default, use the whole search list to
    find the relevant object(s).  }

  \item{signature}{ The class signature of the relevant method.  A
    signature is a named or unnamed vector of character strings.  If
    named, the names must be formal argument names for the generic
    function.  If \code{signature} is unnamed, the default is to use
    the first \code{length(signature)} formal arguments of the
    function. }
  
  \item{file}{ The file on which to dump method definitions. }

  \item{def}{ The function object defining the method; if omitted, the
    current method definition corresponding to the signature. }

  \item{includeDefs, classes, inherited}{See the detailed discussion of
    \code{showMethods}}

  \item{...}{Named or unnamed arguments to form a signature.}

  \item{generic}{In testing or finding functions, should generic
    functions be included.  Supply as \code{FALSE} to get only
    non-generic functions.}

  \item{fdef}{Optional, the generic function definition.

   Usually omitted in calls to \code{isGeneric}}
  
  \item{getName}{If \code{TRUE}, \code{isGeneric} returns the name of
    the generic.  By default, it returns \code{TRUE}. }
  \item{methods}{
    The methods object containing the methods to be dumped.  By default,
    the methods defined for this generic (optionally on the specified
    \code{where} location.
  }
  
}

\details{
  \describe{
  
  \item{\code{setGeneric}:}{ 
  

If there is already a non-generic function of this name, it will be used
to define the generic unless \code{def} is supplied, and the current function will
become the default method for the generic.

If \code{def} is supplied, this defines the generic function, and no default method
will exist (often a good feature, if the function should only be available for
a meaningful subset of all objects).

Arguments \code{group} and \code{valueClass} are retained for consistency with S-Plus, but
are currently not used.

  
  }
  \item{\code{isGeneric}:}{ 
  

If the \code{fdef} argument is supplied, take this as the definition of the
generic, and test whether it is really a generic, with \code{f} as the name of
the generic.  (This argument is not available in S-Plus.)

  
  }
  \item{\code{removeGeneric}:}{ 
  

If \code{where} supplied, just remove the version on this element of the search list;
otherwise, removes the first version encountered.

  
  }
  \item{\code{standardGeneric}:}{ 
  
Generic functions should usually have a call to \code{standardGeneric} as their entire
body.  They can, however, do any other computations as well.

The usual \code{setGeneric} (directly or through calling
\code{setMethod}) creates a function with a call to
\code{standardGeneric}.

  
  }
  \item{\code{getMethods}:}{ 
  If the function is not
a generic function, returns NULL.  
The \code{f} argument can be either the character string name of the generic
or the object itself

The \code{where} argument optionally says where to look for the function, if
\code{f} is given as the name.

  
  }
  \item{\code{dumpMethod}:}{ 
   
The resulting source file will recreate the method.

  
  }
  \item{\code{findFunction}:}{ 
  If \code{generic} is FALSE, ignore generic
functions.

  
  }
  \item{\code{selectMethod}:}{ 
  The vector of strings
for the classes can be named or not.  If named, the names must match formal
argument names of f.  If not named, the signature is assumed to apply to the
arguments of f in order.
mustFind = If TRUE, an error results if there is no method (or no unique method)
corresponding to this signature.  Otherwise may return NULL or a MethodsList
object.

  
  }
  \item{\code{dumpMethods}:}{ 
  

If \code{signature} is supplied only the methods matching this initial signature
are dumped.  (This feature is not found in S-Plus:  don't use it if you want
compatibility.)

  }
  \item{\code{signature}:}{ 
  The advantage
of using \code{signature} is to provide a check on which arguments you meant, as well
as clearer documentation in your method specification.  In addition, \code{signature}
checks that each of the elements is a single character string.

  }
  \item{\code{showMethods}:}{ 
The output style is different from S-Plus in that it does not show the database
from which the definition comes, but can optionally include the method definitions,
if \code{includeDefs} is \code{TRUE}.

If \code{inherits} is \code{TRUE}, then methods that have been found by inheritance, so
far in the session, will be included and marked as inherited.

  
  }
  \item{\code{removeMethods}:}{ 
  Returns \code{TRUE} if
  \code{f} was a generic function, \code{FALSE} (silently) otherwise.
  
  If there is a default method, the function will be re-assigned as
  a simple function with this definition; otherwise, it will be removed.  The
  assignment or removal can be controlled by optional argument \code{where}, which
  defaults to the first element of the search list having a function called \code{f}.
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
