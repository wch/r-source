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
setGeneric(name, def, group=NULL, valueClass=NULL, where=1, doAssign,
           myDispatch = FALSE, useAsDefault)

setGroupGeneric(name, def, group=NULL, valueClass=NULL, knownMembers, where=1)
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
    and default values for the generic are taken from \code{def}.  The
    body will be set to the standard dispatch mechanism, unless you want
    to do something different (use argument \code{myDispatch} in this
    case.
    
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
  \item{where}{ Where to store the resulting initial methods definition,
    and possibly the generic function; by
    default, stored into the global environment.
  }
  \item{doAssign}{ 
    You can supply \code{doAssign} as \code{TRUE} to force assigning a
    generic function even if one already exists. See the details
    section. The rule of not assigning
    generics for primitives cannot be overriden by setting
    \code{doAssign} to \code{TRUE}.
  }
  \item{myDispatch}{
    Normally, the body of a generic function does nothing except to
    dispatch methods (via a call to \code{\link{standardGeneric}}).  The
    evaluation model, however, allows the generic to do any other
    computation it wants, it it calls \code{standardGeneric} at
    some point.  Otherwise, \code{setGeneric} will ignore the body of
    the supplied generic unless \code{myDispatch} is set to
    \code{TRUE}. See the details section.
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
  \code{useAsDefault} does not supply one.  Remember, though, you can
  also just assign the default you want (even one that generates an
  error) rather than relying on the prior situation.

  Usually, calling \code{setGeneric} if there is already a
  \emph{generic} function of this name has no effect.  If you want to
  force a new definition, supply \code{doAssign = TRUE} (but it would
  be cleaner in most cases to remove the old generic before creating
  the new one; see \code{\link{removeGeneric}}).  There is one
  absolute restriction:  you cannot create an explicit generic for the
  primitive functions in the base library.  These are dispatched from
  C code for efficiency and, also, are implicitly not to be redefined
  in any case.

  As mentioned, the body of a generic function usually does nothing
  except for dispatching methods by a call to \code{standardGeneric}.
  Under some circumstances you might just want to do some additional
  computation in the generic function itself.  As long as your
  function eventually calls \code{standardGeneric} that is permissible
  (though perhaps not a good idea, in that it makes the behavior of
  your function different from the usual S model).  If your function
  does \emph{not} call \code{standardGeneric} you are in trouble,
  because none of the methods for the function will ever be
  dispatched.  The computations in \code{setGeneric} look for such a
  call, and normally ignore the body of a \code{def} argument that
  has no such call.  If you really need to supply such a definition
  (perhaps because you call something else that calls
  \code{standardGeneric}), you can force the use of \code{def} by
  supplying \code{myDispatch = TRUE}.
      By default, the generic function can return any object.
    
   If \code{valueClass} is supplied, it can be either a vector of
   class names or an unevaluated expression.  If an unevaluated expression is supplied (e.g., by using
   \code{\link{quote}}), the expression will be evaluated in the frame
   of the call to the generic function, after the method is
   dispatched.  Therefore, the expression can involve any argument or
   locally defined object in that frame.
   The test is done in the form
   \code{is(object, class)}, so that a value from a class that extends
   a supplied value class will be accepted.
   An empty (i.e., zero length) vector of classes means anything is
   allowed (but it would be clearer programming to have an expression that
   evaluated to \code{"ANY"}).

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
\value{
  The \code{setGeneric} function exists for its side effect: saving the
  generic function to allow methods to be specified later.  It returns
  \code{name}.
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\author{
  John Chambers
}
\seealso{
  \code{\link{Methods}} for a discussion of other functions to specify
  and manipulate the methods of generic functions.
}
\keyword{ programming }
\keyword{ methods }
