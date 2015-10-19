% File src/library/methods/man/setGeneric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{setGeneric}
\alias{setGeneric}
\alias{setGroupGeneric}
\title{Define a New Generic Function}
\description{
  Create a new generic function of the given name, that is, a function
  that dispatches methods according to the classes of the arguments,
  from among the formal methods defined for this function.
}
\usage{
setGeneric(name, def= , group=list(), valueClass=character(),
           where= , package= , signature= , useAsDefault= ,
           genericFunction= , simpleInheritanceOnly = )

setGroupGeneric(name, def= , group=list(), valueClass=character(),
                knownMembers=list(), package= , where= )
}
\arguments{
  \item{name}{ The character string name of the generic function.
    The simplest (and recommended) call, \code{setGeneric(name)},
    looks for a function with this name and creates a corresponding
    generic function,  if the function found was not generic.  In the
    latter case, the existing function becomes the default method.
  }
  \item{def}{An optional function object, defining the generic.
    Don't supply this argument if you want to turn an existing non-generic
    function into a generic. In this case, you usually want to use the
    simple call with one argument.

    Do supply \code{def} if there is no
    current function of this name or for some reason you do not want
    to use that function to define the generic. In that case, the
    formal arguments and
    default values for the generic are taken from \code{def}.  In most
    cases, the body of \code{def} will then define the default method,
    as the existing function did in the one-argument call.

    If you want to create a new generic function with \emph{no}
    default method, then \code{def} should be only a call to
    \code{\link{standardGeneric}} with the same character string as
    \code{name}.


  }
  \item{group}{ Optionally, a character string giving the name of the group
    generic function to which this function belongs.  See
    \link{Methods} for details of group generic functions in method selection.
  }
  \item{valueClass}{ An optional character vector of one or more class
    names.  The value returned by the generic function must
    have (or extend) this class, or one of the classes; otherwise,
    an error is generated.
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
    Optionally, the vector of names, from among the formal arguments to
    the function, that can appear in the signature of methods for this
    function, in calls to \code{\link{setMethod}}.  If \code{\dots} is
    one of the formal arguments, it is treated specially.  Starting with
    version 2.8.0 of \R, \code{\dots} may be signature of the generic
    function.  Methods will then be selected if their signature matches
    all the \code{\dots} arguments.  See the documentation for topic
    \link{dotsMethods} for details.  In the present version, it is not
    possible to mix \code{\dots} and other arguments in the signature
    (this restriction may be lifted in later versions).

    By default, the signature is inferred from the implicit generic
    function corresponding to a non-generic function.  If no implicit
    generic function has been defined, the default is all the formal
    arguments except \code{\dots}, in the order they appear in the
    function definition. In the case that \code{\dots} is the only
    formal argument, that is also the default signature.  To use
    \code{\dots} as the signature in a function that has any other
    arguments, you must supply the signature argument explicitly.  See
    the \dQuote{Implicit Generic} section below for more details.
  }
  \item{useAsDefault}{
    Override the usual choice of default argument.
    Argument \code{useAsDefault} can be supplied, either as a function
    to use for the default, or as a logical value.
    This argument is now rarely needed.
    See the section \sQuote{Details}.
  }
  \item{simpleInheritanceOnly}{
    Supply this argument as \code{TRUE} to require that methods selected
    be inherited through simple inheritance only; that is, from
    superclasses specified in the \code{contains=} argument to
    \code{\link{setClass}}, or by simple inheritance to a class union or
    other virtual class.  Generic functions should require simple
    inheritance if they need to be assured that they get the complete
    original object, not one that has been transformed.  Examples of
    functions requiring simple inheritance are \code{\link{initialize}},
    because by definition it must return an object from the same class
    as its argument, and \code{\link{show}}, because it claims to give a
    full description of the object provided as its argument.
  }
  \item{genericFunction}{Don't use; for (possible) internal use only.}

  \item{knownMembers}{
    (For \code{setGroupGeneric} only.)  The names of functions that are
    known to be members of this group.  This information is used to
    reset cached definitions of the member generics when information
    about the group generic is changed.
  }
}
\section{Basic Use}{
  The \code{setGeneric} function is called to initialize a generic
  function as preparation for defining some methods for that function.

  The simplest and most common situation is that \code{name} is already
  an ordinary non-generic non-primitive function, and you now want to
  turn this function into a generic.  In this case you will most often
  supply only \code{name}, for example:

    \code{setGeneric("colSums")}

  There must be an existing function of this name, on some attached
  package (in this case package \code{"base"}).  A generic version of
  this function will be created in the current package (or in the global
  environment if the call to \code{setGeneric()} is from an ordinary
  source file or is entered on the command line).  The existing function
  becomes the default method, and the package slot of the new generic
  function is set to the location of the original function
  (\code{"base"} in the example).  It's an important feature that the
  same generic function definition is created each time, depending in
  the example only on the definition of \code{print} and where it is
  found.  The \code{signature} of the generic function, defining which
  of the formal arguments can be used in specifying methods, is set by
  default to all the formal arguments except \code{\dots}.

  Note that calling \code{setGeneric()} in this form is not strictly
  necessary before calling \code{setMethod()} for the same function.  If
  the function specified in the call to \code{setMethod} is not generic,
  \code{setMethod} will execute the call to \code{setGeneric} itself.
  Declaring explicitly that you want the function to be generic can be
  considered better programming style; the only difference in the
  result, however, is that not doing so produces a message noting the
  creation of the generic function.

  You cannot (and never need to) create an explicit generic version of
  the primitive functions in the base package.  Those which can be
  treated as generic functions have methods selected and dispatched from
  the internal C code, to satisfy concerns for efficiency, and the
  others cannot be made generic.  See the section on Primitive Functions
  below.

  The description above is the effect when the package that owns the
  non-generic function has not created an implicit generic version.
  Otherwise, it is this implicit generic function that is used. See the
  section on Implicit Generic Functions below.  Either way, the
  essential result is that the \emph{same} version of the generic
  function will be created each time.

  The second common use of \code{setGeneric()} is to create a new
  generic function, unrelated to any existing function, and frequently
  having no default method.  In this case, you need to supply a skeleton
  of the function definition, to define the arguments for the function.
  The body of a generic function is usually a standard form,
  \code{standardGeneric(name)} where \code{name} is the quoted name of
  the generic function.  When calling \code{setGeneric} in this form,
  you would normally supply the \code{def} argument as a function of
  this form.  See the second and third examples below.

  The \code{useAsDefault} argument controls the default method for the
  new generic.  If not told otherwise, \code{setGeneric} will try to
  find a non-generic version of the function to use as a default.  So,
  if you do have a suitable default method, it is often simpler to first
  set this up as a non-generic function, and then use the one-argument
  call to \code{setGeneric} at the beginning of this section.  See the
  first example in the Examples section below.

  If you \emph{don't} want the existing function to be taken as default,
  supply the argument \code{useAsDefault}.  That argument can be the
  function you want to be the default method, or \code{FALSE} to force
  no default (i.e., to cause an error if there is no direct or inherited
  method selected for a call to the function).
}
\section{Details}{
  The great majority of calls to \code{setGeneric()} should either
  have one argument to ensure that an existing function can have
  methods, or arguments \code{name} and \code{def} to create a new
  generic function and optionally a default method.  If that's not
  what you plan to do, read on.

  If you want to change the behavior of an existing function (typically,
  one in another package) when you create a generic version, you must
  supply arguments to \code{setGeneric} correspondingly.  Whatever
  changes are made, the new generic function will be assigned with a
  package slot set to the \emph{current} package, not the one in which
  the non-generic version of the function is found.  This step is
  required because the version you are creating is no longer the same as
  that implied by the function in the other package.  A message will be
  printed to indicate that this has taken place and noting one of the
  differences between the two functions.  It tends to be a bad idea,
  because the two versions are now competing for methods, with many
  chances for mistakes in programming.

  The body of a generic function usually does nothing except for
  dispatching methods by a call to \code{standardGeneric}.  Under some
  circumstances you might just want to do some additional computation in
  the generic function itself.  As long as your function eventually
  calls \code{standardGeneric} that is permissible (though perhaps not a
  good idea, in that it may make the behavior of your function less easy
  to understand).  If your explicit definition of the generic function
  does \emph{not} call \code{standardGeneric} you are in trouble,
  because none of the methods for the function will ever be dispatched.

  By default, the generic function can return any object.  If
  \code{valueClass} is supplied, it should be a vector of class names;
  the value returned by a method is then required to satisfy
  \code{is(object, Class)} for one of the specified classes.  An empty
  (i.e., zero length) vector of classes means anything is allowed.  Note
  that more complicated requirements on the result can be specified
  explicitly, by defining a non-standard generic function.

  The \code{setGroupGeneric} function behaves like \code{setGeneric}
  except that it constructs a group generic function, differing in two
  ways from an ordinary generic function.  First, this function cannot
  be called directly, and the body of the function created will contain
  a stop call with this information.  Second, the group generic function
  contains information about the known members of the group, used to
  keep the members up to date when the group definition changes, through
  changes in the search list or direct specification of methods, etc.
}
\section{Implicit Generic Functions}{
  Saying that a non-generic function \dQuote{is converted to a generic}
  is more precisely state that the function is converted to the
  corresponding \emph{implicit} generic function.  If no special action
  has been taken, any function corresponds implicitly to a generic
  function with the same arguments, in which all arguments other than
  \code{\dots} can be used.  The signature of this generic function is the
  vector of formal arguments, in order, except for \code{\dots}.

  The source code for a package can define an implicit generic function
  version of any function in that package (see \link{implicitGeneric}
  for the mechanism).  You can not, generally, define an implicit
  generic function in someone else's package. The usual reason for
  defining an implicit generic is to prevent certain arguments from
  appearing in the signature, which you must do if you want the
  arguments to be used literally or if you want to enforce lazy
  evaluation for any reason.  An implicit generic can also contain some
  methods that you want to be predefined; in fact, the implicit generic
  can be any generic version of the non-generic function.  The implicit
  generic mechanism can also be used to prohibit a generic version (see
  \code{\link{prohibitGeneric}}).

  Whether defined or inferred automatically, the implicit generic will
  be compared with the generic function that \code{setGeneric} creates,
  when the implicit generic is in another package.  If the two functions
  are identical, then the \code{package} slot of the created generic
  will have the name of the package containing the implicit generic.
  Otherwise, the slot will be the name of the package in which the
  generic is assigned.

  The purpose of this rule is to ensure that all methods defined for a
  particular combination of generic function and package names
  correspond to a single, consistent version of the generic function.
  Calling \code{setGeneric} with only \code{name} and possibly
  \code{package} as arguments guarantees getting the implicit generic
  version, if one exists.

  Including any of the other arguments can force a new, local version of
  the generic function.  If you don't want to create a new version,
  don't use the extra arguments.
}
\section{Generic Functions and Primitive Functions}{
  A number of the basic \R functions are specially implemented as
  primitive functions, to be evaluated directly in the underlying C code
  rather than by evaluating an \R language definition.  Most have
  implicit generics (see \code{\link{implicitGeneric}}), and become
  generic as soon as methods (including group methods) are defined on
  them.  Others cannot be made generic.

  Even when methods are defined for such functions, the generic version
  is not visible on the search list, in order that the C version
  continues to be called.  Method selection will be initiated in the C
  code.  Note, however, that the result is to restrict methods for
  primitive functions to signatures in which at least one of the classes
  in the signature is a formal S4 class.

  To see the generic version of a primitive function, use
  \code{\link{getGeneric}(name)}.  The function
  \code{\link{isGeneric}} will tell you whether methods are defined
  for the function in the current session.

  Note that S4 methods can only be set on those primitives which are
  \sQuote{\link{internal generic}}, plus \code{\%*\%}.
}
\value{
  The \code{setGeneric} function exists for its side effect: saving the
  generic function to allow methods to be specified later.  It returns
  \code{name}.
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
\dontshow{
setClass("track", representation(x="numeric", y="numeric"))
}

## create a new generic function, with a default method
setGeneric("props", function(object) attributes(object))

## A new generic function with no default method
setGeneric("increment",
  function(object, step, ...)
    standardGeneric("increment")
)


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

tryIt <- function(expr) tryCatch(expr, error = function(e) e)
stopifnot(identical(authorNames(c("me", "you")), c("me", "you")),
          is(tryIt(authorNames(character())), "error"), # empty value
          is(tryIt(authorNames(NULL)), "error"))        # no default method
}

## An example of group generic methods, using the class
## "track"; see the documentation of 'setClass' for its definition

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
removeGeneric("props")
removeGeneric("increment")
}
}
\seealso{
  \code{\link{Methods}} and the links there for a general discussion,
  \code{\link{dotsMethods}} for methods that dispatch on
  \code{\dots}, and \code{\link{setMethod}} for method definitions.
}
\keyword{ programming }
\keyword{ methods }
