\name{getMethod}
\alias{getMethod}
\alias{getMethods}
\alias{selectMethod}
\alias{MethodsListSelect}
\title{ Get the Definition of a Method }
\description{
  A call to \code{getMethod} or \code{selectMethod} returns the method
  corresponding a particular generic function and signature (set
  classes for formal arguments); \code{selectMethod} makes use of
  inheritance, while \code{getMethod} does not.  The function
  \code{getMethods} returns \emph{all} the methods for the generic function.
}
\usage{
getMethod(f, signature=character(), where=-1, optional=FALSE)

selectMethod(f, signature, optional=False, mlist=getMethods(f))

getMethods(f, where=-1)

MethodsListSelect(f, ev, mustFind = TRUE, ...)
}
\arguments{
  \item{f}{ The character-string name of the generic function }
  \item{signature}{ The signature of classes to match to the arguments of \code{f}.  The vector of strings
      for the classes can be named or not.  If named, the names must match formal
      argument names of \code{f}.  If not named, the signature is assumed to apply to the
      arguments of \code{f} in order.
 }
  \item{where}{ Where to look for the method.  By default, looks in
      all the currently defined methods. }
  \item{optional}{ If the selection does not produce a unique result,
      an error is generated, unless this argument is \code{TRUE}.  In
      that case, the value returned is either a \code{MethodsList}
      object, if more than one method matches this signature, or
      \code{NULL} if no method matches.}

  \item{mlist}{In \code{selectMethod}, the \code{MethodsList} object
      can be explicitly supplied.  (Unlikely to be used, except in the
      recursive call that finds matches to more than one argument.)}

  \item{ev}{The environment in which argument evaluations are done in
    \code{MethodsListSelect}.  Currently must be supplied, but should
    usually be \code{sys.frame(sys.parent())} when calling the function explicitly
    for debugging purposes.}
}
\details{
  The functions \code{getMethod} and \code{selectMethod} usually
  differ only in that the latter includes class inheritance in its
  search, and the former does not.  For example, if class
  \code{"trackCurve"} has been defined to extend class \code{"track"},
  then at any stage of selection, if the signature wants class
  \code{"trackCurve"}, and the methods list has a \code{"track"}
  element but no \code{"trackCurve"} element, then \code{selectMethod}
  will pick that element, but \code{getMethod} will return
  \code{NULL}.

  The other differences between the two functions have to do with
  options on where to look:  \code{getMethod} takes an environment or
  database argument, \code{selectMethod} takes a \code{MethodsList}
  object.

  When \code{selectMethod} is called with a signature having more than
  one argument, it does selection recursively.  Look at the source for
  the function for the precise definition, but the effect is roughly
  as follows.  An element of the \code{MethodsList} object is selected
  according to the first element of the signature (an exact match to
  the corresponding class name, an inherited match, or the default
  (\code{"ANY"}) element.  The result may be a function, a
  \code{MethodsList} object, or \code{NULL} (no match).  In the second
  case, if there are more elements in the signature,
  \code{selectMethod} drops the first element of the signature, and
  calls itself recursively, with that object as the \code{mlist}
  argument.

  The function \code{MethodsListSelect} returns the method selected by
  the evaluator for the specified function, being called from the
  supplied environment.  This is usually, but not always, the same as
  that returned by \code{selectMethod} given the equivalent set of
  classes (i.e., the classes of the arguments used in defining methods
  for this function).  The differences come because inheritance
  relations can be conditional, and \code{selectMethod} does not  have
  that information, since it uses classes rather than actual objects
  from those classes.

  Normally you won't call \code{MethodsListSelect} directly, but it is
  possible to use it for debugging purposes (only for distinctly
  advanced users!).

  Note that the statement that \code{MethodsListSelect} corresponds to the
  selection done by the evaluator is a fact, not an assertion, in the
  sense that the evaluator code constructs and executes a call to
  \code{selectMethod} when it does not already have a cached method
  for this generic function and signature.  (The value returned is
  stored by the evaluator so that the search is not required next time.)
}
\value{
  The call to \code{selectMethod} or \code{getMethod} returns a
  function object, the selected method, if a unique selection exists.
  Otherwise an error is thrown if \code{optional} is \code{FALSE}.  If
  \code{optional} is \code{TRUE}, the value returned is \code{NULL} if
  no method matched, or a \code{MethodsList} object if multiple
  methods matched.

  The call to \code{getMethods} returns the \code{MethodsList} object
  containing all the methods requested.  If there are none,
  \code{NULL} is returned: \code{getMethods} does not generate an
  error in this case.
}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package implement a facility for classes and
methods as described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\examples{

}
\keyword{programming}
\keyword{classes}
\keyword{methods}
