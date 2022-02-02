% File src/library/methods/man/setGeneric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{setGeneric}
\alias{setGeneric}
\title{Create a Generic Version of a Function}
\description{
  Create a generic version of the named function so that methods may
  be defined for it.  A call to \code{\link{setMethod}} will call
  \code{setGeneric} automatically if applied to a non-generic
  function.

  An explicit call to \code{setGeneric} is usually not required, but
  doesn't hurt and makes explicit that methods are being defined for a
  non-generic function.

  Standard calls will be of the form:
  
  \code{setGeneric(name)}

  where \code{name} specifies an existing function, possibly in another
  package.  An alternative when creating a new generic function in this package is:

  \code{setGeneric(name, def)}

  where the function definition \code{def} specifies the formal
  arguments and becomes the default method.

}
\usage{
setGeneric(name, def= , group=list(), valueClass=character(),
           where= , package= , signature= , useAsDefault= ,
           genericFunction= , simpleInheritanceOnly = )
}
\arguments{
  \item{name}{ The character string name of the generic function.
  }
  \item{def}{An optional function object, defining the non-generic
      version, to become the default method.  This is equivalent in
      effect to assigning \code{def} as the function and then using
      the one-argument call to \code{setGeneric}.

      \emph{The following arguments are specialized, optionally used
        when creating a new generic function with non-standard
        features. They should not be used when the non-generic is in
        another package.}

  }
  \item{group}{ The name of the group
    generic function to which this function belongs.  See
    \link{Methods_Details} for details of group generic functions in method
    selection and \link{S4groupGeneric} for existing groups.
  }
  \item{valueClass}{ A character vector specifying one or more class
    names.  The value returned by the generic function must
    have (or extend) this class, or one of the classes; otherwise,
    an error is generated.
  }
  \item{signature}{
    The vector of names from among the formal arguments to
    the function, that will be allowed in the signature of methods for this
    function, in calls to \code{\link{setMethod}}.  By default and
    usually, this will be all formal arguments except \code{\dots}.

    A non-standard signature for the generic function may be
    used to exclude arguments that take advantage of lazy evaluation;
    in particular, if the argument may \emph{not} be evaluated then it
    cannot be part of the signature.

    While \code{\dots} cannot be used as part of a general signature,
    it is possible to have this as the \emph{only} element of the
    signature.
    Methods will then be selected if their signature matches
    all the \code{\dots} arguments.  See the documentation for topic
    \link{dotsMethods} for details.  It is not
    possible to mix \code{\dots} and other arguments in the signature.

    It's usually a mistake to omit arguments from the signature in the
    belief that this improves efficiency.  For method selection, the
    arguments that are used in the signatures for the \emph{methods}
    are what counts, and then only seriously on the first call to the
    function with that combination of classes.
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
  \item{useAsDefault}{
    Override the usual default method mechanism.  Only relevant when
    defining a nonstandard generic function.
    See the section \sQuote{Specialized Local Generics}.

    \emph{The remaining arguments are obsolete for normal applications.}
  }
  \item{package}{ The name of the package with which this function is
    associated.  Should be determined automatically from the
    non-generic version.
     
  }
  \item{where}{ Where to store the resulting objects as side effects.
      The default, to store in the package's namespace, is the only
      safe choice.
  }
  \item{genericFunction}{Obsolete.}
}
\section{Basic Use}{
  The \code{setGeneric} function is called to initialize a generic
  function as preparation for defining some methods for that function.

  The simplest and most common situation is that \code{name} specifies
  an existing function, usually in another package. You now want to
  define methods for this function.  In this case you should
  supply only \code{name}, for example:

    \code{setGeneric("colSums")}

  There must be an existing function of this name (in this case in
  package \code{"base"}).  The non-generic function can be in the same
  package as the call, typically the case when you are creating a new
  function plus methods for it. When the function is in
  another package, it must be available by name, for
  example through an \code{importFrom()} directive in this package's
  \code{NAMESPACE} file. Not required for functions in \code{"base"},
  which are implicitly imported.

  A generic version of
  the function will be created in the current package.  The existing function
  becomes the default method, and the package slot of the new generic
  function is set to the location of the original function
  (\code{"base"} in the example).  

  Two special types of non-generic should be noted.
  Functions that dispatch S3 methods by calling
  \code{\link{UseMethod}} are ordinary functions, not objects from the
  \code{"genericFunction"} class.  They are made generic like any
  other function, but some special considerations apply to ensure that
  S4 and S3 method dispatch is consistent (see \link{Methods_for_S3}).

  Primitive functions are handled in C code and don't exist as normal
  functions.
  A call to \code{setGeneric} is allowed in the simple form, but no
  actual generic function object is created.  Method dispatch will
  take place in the C code. See the section on Primitive Functions for
  more details.

  It's an important feature that the
  identical generic function definition is created in every package that
  uses the same \code{setGeneric()} call.
  When any of these packages is loaded into an \R session, this
  function will be added to a table of generic functions, and will
  contain a methods table of all the available methods for the
  function.

  Calling \code{setGeneric()} is not strictly
  necessary before calling \code{setMethod()}.  If
  the function specified in the call to \code{setMethod} is not generic,
  \code{setMethod} will execute the call to \code{setGeneric} itself.
  In the case that the non-generic is in another package, does not
  dispatch S3 methods and is not a primitive, a message is printed noting the
  creation of the generic function the first time \code{setMethod} is called.

  The second common use of \code{setGeneric()} is to create a new
  generic function, unrelated to any existing function.  See the
  \code{asRObject()} example below.
  This case can be handled just like the previous examples, with only
  the difference that the non-generic function exists in the
  current package.
  Again, the non-generic version becomes the default method.
  For clarity it's best for the assignment to immediately precede the
  call to \code{setGeneric()} in the source code.

  Exactly the same result can be obtained by supplying the default as
  the \code{def} argument instead of assigning it.
  In some applications, there will be no completely general default
  method. While there is a special mechanism for this (see the
  \sQuote{Specialized Local Generics} section), the recommendation is to provide a
  default method that signals an error, but with a message that
  explains as clearly as you can why a non-default method is needed.

}
\section{Specialized Local Generics}{
  The great majority of calls to \code{setGeneric()} should either
  have one argument to ensure that an existing function can have
  methods, or arguments \code{name} and \code{def} to create a new
  generic function and optionally a default method.

  It is possible to create generic functions with nonstandard
  signatures, or functions that do additional computations besides
  method dispatch or that belong to a group of generic functions.

  None of these mechanisms should be used with a non-generic function
  from a \emph{different} package, because the result is to create a
  generic function that may not be consistent from one package to another.
  When any such options are used,
  the new generic function will be assigned with a
  package slot set to the \emph{current} package, not the one in which
  the non-generic version of the function is found.

  There is a mechanism to define a specialized generic version of a
  non-generic function, the \code{\link{implicitGeneric}}
  construction.
  This defines the generic version, but then reverts the function to
  it non-generic form, saving the implicit generic in a table to be
  activated when methods are defined.
  However, the mechanism can only legitimately be used either for a non-generic
  in the same package or by the \code{"methods"} package itself.
  And in the first case, there is no compelling reason not to simply
  make the function generic, with the non-generic as the default
  method.
  See \code{\link{implicitGeneric}} for details.

  The body of a generic function usually does nothing except for
  dispatching methods by a call to \code{standardGeneric}.  Under some
  circumstances you might just want to do some additional computation in
  the generic function itself.  As long as your function eventually
  calls \code{standardGeneric} that is permissible.
  See the example \code{"authorNames"} below.

  In this case, the \code{def} argument will define the nonstandard
  generic, not the default method.
  An existing non-generic of the same name and calling sequence should
  be pre-assigned.  It will become the default method, as usual.
  (An alternative is the \code{useAsDefault} argument.)

  By default, the generic function can return any object.  If
  \code{valueClass} is supplied, it should be a vector of class names;
  the value returned by a method is then required to satisfy
  \code{is(object, Class)} for one of the specified classes.  An empty
  (i.e., zero length) vector of classes means anything is allowed.  Note
  that more complicated requirements on the result can be specified
  explicitly, by defining a non-standard generic function.

  If the \code{def} argument calls \code{standardGeneric()} (with or
  without additional computations) and there is no existing
  non-generic version of the function, the generic is created without
  a default method.  This is not usually a good idea:  better to have a
    default method that signals an error with a message explaining why
    the default case is not defined.

  A new generic function can be created belonging to an existing group
  by including the \code{group} argument.  The argument list of the
  new generic must agree with that of the group. See
  \code{\link{setGroupGeneric}} for defining a new group generic.
   For the role of group generics in
  dispatching methods, see \link{GroupGenericFunctions} and section
  10.5 of the second reference.


}

\section{Generic Functions and Primitive Functions}{
  A number of the basic \R functions are specially implemented as
  primitive functions, to be evaluated directly in the underlying C code
  rather than by evaluating an \R language definition.  Most have
  implicit generics (see \code{\link{implicitGeneric}}), and become
  generic as soon as methods (including group methods) are defined on
  them.  Others cannot be made generic.


  Calling \code{setGeneric()} for
  the primitive functions in the base package differs in that it does not, in fact,
  generate an explicit generic function.
  Methods for primitives are selected and dispatched from
  the internal C code, to satisfy concerns for efficiency.
  The same is true for a few
  non-primitive functions that dispatch internally. These include
  \code{unlist} and \code{as.vector}.

  Note, that the implementation restrict methods for
  primitive functions to signatures in which at least one of the classes
  in the signature is a formal S4 class.
  Otherwise the internal C code will not look for methods.
  This is a desirable restriction in principle, since optional
  packages should not be allowed to change the behavior of basic R
  computations on existing data types.

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
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)

 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer. (Section 10.5 for some details.)

}
\examples{

## Specify that this package will define methods for plot()
setGeneric("plot")

## create a new generic function, with a default method
setGeneric("props", function(object) attributes(object))

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

## the asRObject generic function, from package XR
## Its default method just returns object
## See the reference, Chapter 12 for methods

setGeneric("asRObject", function(object, evaluator) {
        object
})


\dontshow{
setMethod("authorNames", "character", function(text)text)

tryIt <- function(expr) tryCatch(expr, error = function(e) e)
stopifnot(identical(authorNames(c("me", "you")), c("me", "you")),
          is(tryIt(authorNames(character())), "error"), # empty value
          is(tryIt(authorNames(NULL)), "error"))        # no default method
}
\dontshow{
removeGeneric("authorNames")
removeGeneric("props")
removeGeneric("asRObject")
}
}
\seealso{
  \code{\link{Methods_Details}} and the links there for a general discussion,
  \code{\link{dotsMethods}} for methods that dispatch on
  \code{\dots}, and \code{\link{setMethod}} for method definitions.
}
\keyword{ programming }
\keyword{ methods }


  % The description above is the effect when the package that owns the
  % non-generic function has not created an implicit generic version.
  % Otherwise, it is this implicit generic function that is used. See the
  % section on Implicit Generic Functions below.  Either way, the
  % essential result is that the \emph{same} version of the generic
  % function will be created each time.

  % The \code{useAsDefault} argument controls the default method for the
  % new generic.  If not told otherwise, \code{setGeneric} will try to
  % find a non-generic version of the function to use as a default.  So,
  % if you do have a suitable default method, it is often simpler to first
  % set this up as a non-generic function, and then use the one-argument
  % call to \code{setGeneric} at the beginning of this section.  See the
  % first example in the Examples section below.

  % If you \emph{don't} want the existing function to be taken as default,
  % supply the argument \code{useAsDefault}.  That argument can be the
  % function you want to be the default method, or \code{FALSE} to force
  % no default (i.e., to cause an error if there is no direct or inherited
  % method selected for a call to the function).
