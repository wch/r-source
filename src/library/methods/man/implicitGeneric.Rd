% File src/library/methods/man/implicitGeneric.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{implicitGeneric}
\alias{implicitGeneric}
\alias{setGenericImplicit}
\alias{prohibitGeneric}
\alias{registerImplicitGenerics}
\alias{implicit generic}
\title{Manage Implicit Versions of Generic Functions}
\description{
  Create or access implicit generic functions, used to enforce
  consistent generic versions of functions that are not currently
  generic.  Function \code{implicitGeneric()} returns the implicit
  generic version, \code{setGenericImplicit()} turns a generic implicit,
  \code{prohibitGeneric()} prevents your function from being made
  generic, and \code{registerImplicitGenerics()} saves a set of implicit
  generic definitions in the cached table of the current session.
}
\usage{
implicitGeneric(name, where, generic)
setGenericImplicit(name, where, restore = TRUE)
prohibitGeneric(name, where)
registerImplicitGenerics(what, where)
}

\arguments{
  \item{name}{ Character string name of the function.}
  \item{where}{ Package or environment in which to register the implicit
    generics.  When using the functions from the top level of your own
    package source, this argument can usually be omitted (and should
    be).}
  \item{generic}{ Optionally, the generic function definition to be
    cached, but usually omitted.   See Details section.}
  \item{restore}{Should the non-generic version of the function be
    restored after the current.}
  \item{what}{For \code{registerImplicitGenerics()}, Optional table of
    the implicit generics to register, but nearly always omitted.   See
    Details section.}
}
\details{
  Multiple packages may define methods for the same function, using the
  version of a function stored in one package.  All these methods should
  be marshaled and dispatched consistently when a user calls the
  function.  For consistency, the generic version of the function must
  have a unique definition (the same arguments allowed in methods
  signatures, the same values for optional slots such as the value
  class, and the same standard or non-standard definition of the
  function itself).

  If the original function is already an S4 generic, there is no
  problem.  The implicit generic mechanism enforces consistency when the
  version in the package owning the function is \emph{not} generic.  If
  a call to \code{\link{setGeneric}()} attempts to turn a function in
  another package into a generic, the mechanism compares the proposed
  new generic function to the implicit generic version of that
  function. If the two agree, all is well. If not, and if the function
  belongs to another package, then the new generic will not be
  associated with that package.  Instead, a warning is issued and a
  separate generic function is created, with its package slot set to the
  current package, not the one that owns the non-generic version of the
  function. The effect is that the new package can still define methods
  for this function, but it will not share the methods in other
  packages, since it is forcing a different definition of the generic
  function.

  The right way to proceed in nearly all cases is to call
  \code{\link{setGeneric}("foo")}, giving \emph{only} the name of the
  function; this will automatically use the implicit generic version.
  If you don't like that version, the best solution is to convince the
  owner of the other package to agree with you and to insert code to
  define the non-default properties of the function (even if the owner
  does not want \code{foo()} to be a generic by default).

  For any function, the implicit generic form is a standard generic in
  which all formal arguments, except for \code{\dots}, are allowed in
  the signature of methods. If that is the suitable generic for a
  function, no action is needed. If not, the best mechanism is to set up
  the generic in the code of the package owning the function, and to
  then call \code{setGenericImplicit()} to record the implicit generic
  and restore the non-generic version.  See the example.

  Note that the package can define methods for the implicit generic as
  well; when the implicit generic is made a real generic, those methods
  will be included.

  Other than predefining  methods, the usual reason for having a
  non-default implicit generic is to provide a non-default signature,
  and the usual reason for \emph{that} is to allow lazy evaluation of
  some arguments.  See the example.  All arguments in the signature of a
  generic function must be evaluated at the time the function needs to
  select a method.  (But those arguments can be missing, with or without
  a default expression being defined; you can always examine
  \code{missing(x)} even for arguments in the signature.)

  If you want to completely prohibit anyone from turning your function
  into a generic, call \code{prohibitGeneric()}.
}
\value{
  Function \code{implicitGeneric()} returns the implicit generic
  definition (and caches that definition the first time if it has to
  construct it).

  The other functions exist for their side effect and return nothing
  useful.
}
\seealso{\code{\link{setGeneric}}}
\examples{

### How we would make the function \link{with}() into a generic:

## Since the second argument, 'expr' is used literally, we want
## with() to only have "data" in the signature.

## Note that 'methods'-internal code now has already extended  with()
## to do the equivalent of the following
\dontrun{
setGeneric("with", signature = "data")
## Now we could predefine methods for "with" if we wanted to.

## When ready, we store the generic as implicit, and restore the original
setGenericImplicit("with")

## (This example would only work if we "owned" function with(),
##  but it is in base.)}

implicitGeneric("with")
}

\keyword{programming}
\keyword{methods}
