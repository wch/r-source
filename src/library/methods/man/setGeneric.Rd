\name{setGeneric}
\alias{setGeneric}
\alias{setGroupGeneric}
\title{Define a New Generic Function}
\description{
 Create a new generic function of the given name, for which formal methods can
 then be defined.
}
\usage{
setGeneric(name, def, group=NULL, valueClass=NULL, where=1, doAssign)

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
    and default values for the generic are taken from \code{def}.  See
    also argument \code{useAsDefault}.
  }
  \item{group}{ Optionally, a character string giving the group of
    generic functions to which this function belongs.  Methods can be
    defined for the corresponding group generic, and these will then
    define methods for this specific generic function, if no method has
    been explicitly defined for the corresponding signature.  See the
    references for more discussion.
  }
  \item{valueClass}{ An optional character string defining the class to
    asserted to be given to the value returned by this generic
    function. By default, the generic function can return any object.
    (At the moment, the assertion supplied in this argument is not
    enforced or checked.)
   }
  \item{where}{ Where to store the resulting initial methods definition,
    and possibly the generic function; by
    default, stored into the global environment.
  }
  \item{doAssign}{ Should a new generic version of the function be
    assigned?  The default action depends on whether a generic version
    currently exists.  If not, one will be assigned to \code{where},
    except that primitive functions in the base package are never turned
    into explicit generic functions (they are dispatched from the
    internal code in the evaluator).

    You can supply \code{doAssign} as \code{TRUE} to force assigning a
    generic function even if one already exists, or as \code{FALSE} to
    prevent assigning the generic in any case (for example, because you
    know one exists on another package).  But the rule of not assigning
    generics for primitives cannot be overriden by setting
    \code{doAssign} to \code{TRUE}.
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
  prepartion for defining some methods for that function.

  The simplest and most common situation is that \code{name} is already
  a function, either an ordinary non-generic function or else a generic
  function in another environment.  In the second case, essentially all
  you will be doing is to make a copy of the existing generic, without
  its methods, in the current environment.  \emph{Only} the \code{name}
  argument will be used.  The new generic \emph{must} agree with the
  current one in essentially everything except for the methods defined.

  The other simple and common situation is that you already have a
  non-generic function (probably in the same place you plan to save the
  generic), and now want to turn this function into a generic.  In this
  case, too, you will most often supply only \code{name}.  The existing
  function becomes the default method, and the special \code{group} and
  \code{valueClass} properties remain unspecified.

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
  The \code{setGeneric} function exists for its side effect:  saving the
  generic function to allow methods to be specified later.  It returns \code{name}.
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

\seealso{
  \code{\link{Methods}} for a discussion of other functions to
  specify and manipulate the methods of generic functions.
}

\keyword{ programming }
\keyword{ methods }
