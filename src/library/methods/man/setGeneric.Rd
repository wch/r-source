\name{setGeneric}
\alias{setGeneric}
\alias{setGroupGeneric}
\title{Define a New Generic Function}
\description{
  Create a new generic function of the given name, for which formal
  methods can then be defined.  Typically, an existing non-generic
  function becomes the default method, but there is much optional
  control.  See the details section.
}
\usage{
setGeneric(name, def= , group=list(), valueClass=character(), where= ,
           package= , signature= , useAsDefault= , genericFunction= )

setGroupGeneric(name, def= , group=list(), valueClass=character(),
                knownMembers=list(), package= , where= )
}
\arguments{
  \item{name}{ The character string name of the generic function.  In
    the simplest and most common case, a function of this name is
    already defined.  The existing function may be non-generic or
    already a generic (see the details).
  }
  \item{def}{ An optional function object, defining the generic.  This
    argument is usually only needed (and is then required) if there is
    no current function of this name. In that case, the formal arguments
    and default values for the generic are taken from \code{def}.  You
    can also supply this argument if you want the generic function to
    do something other than just dispatch methods (an advanced topic
    best left alone unless you are sure you want it).

    Note that \code{def} is \emph{not} the default method; use argument
    \code{useAsDefault} if you want to specify the default separately.
  }
  \item{group}{ Optionally, a character string giving the group of
    generic functions to which this function belongs.  Methods can be
    defined for the corresponding group generic, and these will then
    define methods for this specific generic function, if no method has
    been explicitly defined for the corresponding signature.  See the
    references for more discussion.
  }
  \item{valueClass}{ An optional character vector or unevaluated
      expression.  The value returned by the generic function must
      have (or extend) this class, or one of the classes; otherwise,
      an error is generated.  See the
      details section for supplying an expression.
   }
  \item{package}{ The name of the package with which this function is
      associated.  Usually determined automatically (as the package
      containing the non-generic version if there is one, or else the
      package where this generic is to be saved).
      }
  \item{where}{ Where to store the resulting initial methods definition,
    and possibly the generic function; by
    default, stored into the top-level environment.
  }
  \item{signature}{
      Optionally, the signature of arguments in the function that can
      be used in methods for this generic.  By default, all arguments
      other than \code{...} can be used.  The signature argument can
      prohibit methods from using some arguments.  The argument, if
      provided, is a vector of formal argument names.
  }
  \item{genericFunction}{ The object to be used as a (nonstandard)
  generic function definition. Supply this explicitly \emph{only} if
  you know what you are doing!
  }
  \item{useAsDefault}{
    Override the usual choice of default argument (an existing
    non-generic function or no default if there is no such function).
    Argument \code{useAsDefault} can be supplied,
    either as a function to use for the default, or as a logical value.
    \code{FALSE} says not to have a default method at all, so that an
    error occurs if there is not an explicit or inherited method for a
    call.
    \code{TRUE} says to use the existing function as default,
    unconditionally (hardly ever needed as an explicit argument).
    See the section on details.
    }

  \item{knownMembers}{
    (For \code{setGroupGeneric} only)  The names of functions that are known to be
    members of this group.  This information is used to reset cached
    definitions of the member generics when information about the group
    generic is changed.
    }
}
\details{
  The \code{setGeneric} function is called to initialize a generic
  function in an environment (usually the global environment), as
  preparation for defining some methods for that function.

  The simplest and most common situation is that \code{name} is already
  an ordinary non-generic function, and you now want to turn this
  function into a generic.
  In this
  case you will most often supply only \code{name}.  The existing
  function becomes the default method, and the special \code{group} and
  \code{valueClass} properties remain unspecified.

  A second situation is that you want to create a new, generic
  function, unrelated to any existing function.  In this case, you
  need to supply a skeleton of the function definition, to define the
  arguments for the function.  The body of a generic function is
  usually a standard form, \code{standardGeneric(name)} where
  \code{name} is the quoted name of the generic function.

  When calling \code{setGeneric} in this form, you would normally
  supply the \code{def} argument as a function of this form.  If not
  told otherwise, \code{setGeneric} will try to find a non-generic
  version of the function to use as a default.  If you don't want this
  to happen, supply the argument \code{useAsDefault}.  That argument
  can be the function you want to be the default method.  You can supply
  the argument as \code{FALSE} to force no default (i.e., to cause an error if
  there is not direct or inherited method on call to the function).

  The same no-default situation occurs if there is no non-generic form of the function, and
  \code{useAsDefault=FALSE}.  Remember, though, you can
  also just assign the default you want (even one that generates an
  error) rather than relying on the prior situation.

  You cannot (and never need to) create an explicit generic for the
  primitive functions in the base library.  These are dispatched from
  C code for efficiency and are not to be redefined
  in any case.

  As mentioned, the body of a generic function usually does nothing
  except for dispatching methods by a call to \code{standardGeneric}.
  Under some circumstances you might just want to do some additional
  computation in the generic function itself.  As long as your
  function eventually calls \code{standardGeneric} that is permissible
  (though perhaps not a good idea, in that it makes the behavior of
  your function different from the usual S model).  If your explicit
  definition of the generic function
  does \emph{not} call \code{standardGeneric} you are in trouble,
  because none of the methods for the function will ever be
  dispatched.

      By default, the generic function can return any object.
   If \code{valueClass} is supplied, it should be a vector of
   class names; the value returned by a method is then required to
   satisfy \code{is(object, Class)} for one of the specified classes.
   An empty (i.e., zero length) vector of classes means anything is
   allowed.
   Note that more complicated requirements on the result can be
   specified explicitly, by defining a non-standard generic function.

  The \code{setGroupGeneric} function behaves like \code{setGeneric} except that
  it constructs a group generic function, differing in two ways from an
  ordinary generic function.  First,
  this function cannot be called directly, and the body of the function
  created will contain a stop call with this information.  Second, the
  group generic function contains information about the known members of
  the group, used to keep the members up to date when the group
  definition changes, through changes in the search list or direct
  specification of methods, etc.
}
\section{Generic Functions and Primitive Functions}{
  A number of the basic \R functions are specially implemented as
  primitive functions, to be evaluated directly in the underlying C
  code rather than by evaluating an S language definition.  Primitive
  functions are eligible to have methods, but are handled differently
  by \code{setGeneric} and \code{setGroupGeneric}.  A call to
  \code{setGeneric} for a primitive function does not create a new
  definition of the function, and the call is allowed only to
  \dQuote{turn on} methods for that function.

  A call to \code{setGeneric} for a primitive causes the evaluator to
  look for methods for that generic; a call to \code{setGroupGeneric}
  for any of the groups that include primitives (\code{"Arith", "Logic",
    "Compare", "Ops", "Math", "Math2", "Summary",} and \code{"Complex"},
  see \code{\link{S4groupGeneric}})
  does the same for each of the functions in that group.

  You usually only need to use either function if the methods are
  being defined only for the group generic.  Defining a method for a
  primitive function, say \code{"+"}, by a call to \code{setMethod}
  turns on method dispatch for that function.  But in \R defining a
  method for the corresponding group generic, \code{"Arith"}, does not
  currently turn on method dispatch (for efficiency reasons).  If
  there are no non-group methods for the functions, you have two
  choices.

  You can turn on method dispatch for \emph{all} the functions in the
  group by calling \code{setGroupGeneric("Arith")}, or you can turn on
  method dispatch for only some of the functions by calling
  \code{setGeneric("+")}, etc.  Note that in either case you should
  give the name of the generic function as the only argument.
}
\value{
  The \code{setGeneric} function exists for its side effect: saving the
  generic function to allow methods to be specified later.  It returns
  \code{name}.
}
\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details see \code{?\link{Methods}}
  and the links from that documentation.
}
\examples{
\dontshow{
setClass("track",
            representation(x="numeric", y="numeric"))
}

###   A non-standard generic function.  It insists that the methods
###   return a non-empty character vector (a stronger requirement than
###    valueClass = "character" in the call to setGeneric)

setGeneric("authorNames",
    function(text) {
      value <- standardGeneric("authorNames")
      if(!(is(value, "character") && any(nchar(value)>0)))
        stop("authorNames methods must return non-empty strings")
      value
      })

\dontshow{
setMethod("authorNames", "character", function(text)text)

stopifnot(identical(authorNames(c("me", "you")), c("me", "you")),
          is(trySilent(authorNames(character())), "try-error"), # empty value
          is(trySilent(authorNames(NULL)), "try-error"),        # no default method
          is(trySilent(setGeneric("[", function(x,...)standardGeneric("["))),
             "try-error"))                       # trying setGeneric on a primitive
}

## An example of group generic methods, using the class
## "track"; see the documentation of setClass for its definition

## define a method for the Arith group

setMethod("Arith", c("track", "numeric"),
 function(e1, e2) {
  e1@y <- callGeneric(e1@y , e2)
  e1
})

setMethod("Arith", c("numeric", "track"),
 function(e1, e2) {
  e2@y <- callGeneric(e1, e2@y)
  e2
})

## now arithmetic operators  will dispatch methods:

t1 <- new("track", x=1:10, y=sort(stats::rnorm(10)))

t1 - 100
1/t1

\dontshow{
removeGeneric("authorNames")
removeClass("track")
removeMethods("Arith")
}
}
\seealso{
  \code{\link{Methods}} for a discussion of other functions to specify
  and manipulate the methods of generic functions.
}
\keyword{ programming }
\keyword{ methods }
