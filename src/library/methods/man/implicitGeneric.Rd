% File src/library/methods/man/implicitGeneric.Rd
% Part of the R package, https://www.R-project.org
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
  The implicit generic mechanism stores generic versions of
  functions
  in a table in a package. The package does not want the current
  version of the function to be a generic, however, and retains the
  non-generic version.

  When a call to \code{\link{setMethod}} or
  \code{\link{setGeneric}} creates a generic version for one of these
  functions, the object in the table is used.
  This mechanism is only needed if special arguments were used to
  create the generic; e.g., the \code{signature} or the \code{valueClass}
  options.

    Function \code{implicitGeneric()} returns the implicit
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
    package source, this argument should be omitted.}
  \item{generic}{ Obsolete, and likely to be deprecated.}
  \item{restore}{Should the non-generic version of the function be
    restored?.}
  \item{what}{Optional table of
    the implicit generics to register, but nearly always omitted, when
    it defaults to a standard metadata name.}
}
\details{

  Multiple packages may define methods for the same function, to apply
  to classes defined in that package.  Arithmetic and other operators,
  \code{plot()} and many other basic computations are typical
  examples. It's essential that all such packages write methods for
  the \emph{same} definition of the generic function.  So long as that
  generic uses the default choice for signature and other parameters,
  nothing needs to be done.

  If the generic has special properties, these need to be ensured for
  all packages creating methods for it.  The simplest solution is just
  to make the function generic in the package that originally owned
  it.  If for some reason the owner(s) of that package are unwilling
  to do this, the alternative is to define the correct generic,
  save it in a special table and restore the non-generic version by
  calling \code{setGenericImplicit}.
  

  Note that the package containing the function can define methods for the implicit generic as
  well; when the implicit generic is made a real generic, those methods
  will be included.

  The usual reason for having a
  non-default implicit generic is to provide a non-default signature,
  and the usual reason for \emph{that} is to allow lazy evaluation of
  some arguments.  All arguments in the signature of a
  generic function must be evaluated at the time the function needs to
  select a method.
  In the base function \code{with()} in the example below, evaluation of the argument
  \code{expr} must be delayed; therefore, it is excluded from the signature.

  If you want to completely prohibit anyone from turning your function
  into a generic, call \code{prohibitGeneric()}.

  Function \code{implicitGeneric()} returns the implicit generic
  version of the named function.  If there is no table of these or if
  this function is not in the table, the result of a simple call
  \code{setGeneric(name)} is returned.
}

\section{Implicit Generics for Base Functions}{
Implicit generic versions exist for some functions in the packages
supplied in the distribution of \R itself.  These are stored in the
\sQuote{methods} package itself and will always be available.

As emphasized repeatedly in the documentation,
\code{\link{setGeneric}()} calls for a function in  another package
should never have non-default settings for arguments such as
\code{signature}.
The reasoning applies specially to functions in supplied packages,
since methods for these are likely to exist in multiple packages.
A call to \code{implicitGeneric()} will show the generic version.
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

### How we would make the function with() into a generic:

## Since the second argument, 'expr' is used literally, we want
## with() to only have "data" in the signature.

\dontrun{
setGeneric("with", signature = "data")
## Now we could predefine methods for "with" if we wanted to.

## When ready, we store the generic as implicit, and restore the
original

setGenericImplicit("with")
}

implicitGeneric("with")

# (This implicit generic is stored in the 'methods' package.)
}
\keyword{programming}
\keyword{methods}
