% File src/library/methods/man/setLoadActions.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-2014 R Core Team
% Distributed under GPL 2 or later

\name{setLoadActions}
\alias{setLoadAction}
\alias{setLoadActions}
\alias{getLoadActions}
\alias{hasLoadAction}
\alias{evalOnLoad}
\alias{evalqOnLoad}
\title{
Set Actions For Package Loading
}
\description{
  These functions provide a mechanism for packages to specify
  computations to be done during the loading of the package namespace.
  Such actions are a flexible way to provide information only available at
  load time (such as locations in a dynamically linked library).


  A call to \code{setLoadAction()} or \code{setLoadActions()} specifies
  one or more functions to be called when the corresponding namespace is
  loaded, with the \dots{} argument names being used as identifying
  names for the actions.

  \code{getLoadActions} reports the currently defined load actions,
  given a package's namespace as its argument.

  \code{hasLoadAction} returns \code{TRUE} if a load action
  corresponding to the given name has previously been set for the
  \code{where} namespace.

  \code{evalOnLoad()} and \code{evalqOnLoad()} schedule a specific
  expression for evaluation at load time.
}
\usage{

setLoadAction(action, aname=, where=)

setLoadActions(..., .where=)

getLoadActions(where=)

hasLoadAction(aname, where=)

evalOnLoad(expr, where=, aname=)

evalqOnLoad(expr, where=, aname=)

}
\arguments{

  \item{action, \dots}{
functions of one or more arguments, to be called when this package is
loaded. The functions will be called with one argument (the package
namespace) so all following arguments must have default values.

If the elements of \dots{} are named, these names will be used for the
corresponding load metadata.
}

  \item{where, .where}{
the namespace of the package for which the list of load actions are
defined. This argument is
normally omitted if the call comes from the source code for the package itself, but will
be needed if a package supplies load actions for another package.
}

\item{aname}{the name for the action.  If an action is set without
    supplying a name,  the
    default uses the position in the sequence of actions specified
    (\code{".1"}, etc.).
}

\item{expr}{an expression to be evaluated in a load action in
    environment \code{where}.  In the case of \code{evalqOnLoad()},
    the expression is interpreted literally, in that of
    \code{evalOnLoad()} it must be precomputed, typically as an object
    of type \code{"language"}.
}
}
\details{
The \code{evalOnLoad()} and \code{evalqOnLoad()} functions are for
convenience.
They construct a function to evaluate the expression and call
\code{setLoadAction()} to schedule a call to that function.

Each of the functions supplied as an argument to \code{setLoadAction()} or
\code{setLoadActions()} is saved as metadata in the
namespace, typically that of the package containing the call to
\code{setLoadActions()}.
When this package's namespace is loaded, each of these functions will
be called.
Action functions are called in the order they are supplied to
\code{setLoadActions()}.
The objects assigned have metadata names constructed from the names
supplied in the call; unnamed arguments are taken to be named by their
position in the list of actions (\code{".1"}, etc.).

Multiple calls to \code{setLoadAction()} or  \code{setLoadActions()} can be used in a package's
code; the actions will be scheduled after any previously specified,
except if the name given to \code{setLoadAction()} is that of an
existing action.
In typical applications, \code{setLoadActions()} is more convenient
when calling from the package's own code to set several actions.
Calls to \code{setLoadAction()} are more convenient if the action name
is to be constructed, which is more typical when one package
constructs load actions for another package.

Actions can be revised by assigning with the same name, actual or
constructed, in a subsequent call.
The replacement must still be a valid function, but can of course do
nothing if the intention was to remove a previously specified action.

The functions must have at least one argument.  They will be called with
one argument, the namespace of the package.  The functions will be
called at the end of processing of S4 metadata, after dynamically
linking any compiled code, the call to \code{.onLoad()}, if any, and
caching method and class definitions, but before the namespace is
sealed.

Functions may therefore assign or modify objects in the namespace
supplied as the argument in the call.
The mechanism allows packages to save information not available until
load time, such as values obtained from a dynamically linked library.

Load actions should be contrasted with user load hooks supplied by
\code{\link{setHook}()}.
User hooks are generally provided from outside the package and are run
after the namespace has been sealed.
Load actions are part of the package code, and the list of actions is
normally established when the package is installed.


Load actions can be supplied directly in the source code for a package.
It is also possible and useful to provide facilities in one package to
create load actions in another package.
The software needs to be careful to assign the action
functions in the correct environment, namely the namespace of the target
package.

}
\value{
\code{setLoadAction()}  and \code{setLoadActions()} are called for their side effect and return no
useful value.

\code{getLoadActions()} returns a named list of the actions in the
supplied namespace.

\code{hasLoadAction()} returns \code{TRUE} if the specified action
name appears in the actions for this package.
}

\seealso{
  \code{\link{setHook}} for safer (since they are run after the
  namespace is sealed) and more comprehensive versions in the
  base package.
}

\examples{
\dontrun{
## in the code for some package

## ... somewhere else
setLoadActions(function(attach)
   cat(c("Loaded", "Unloaded")[attach], "at", Sys.time(), "\n"),
  setCount = function(ns) assign("myCount", 1, envir = ns),
  function(ns) assign("myPointer", getMyExternalPointer(), envir = ns))
  ... somewhere later
if(countShouldBe0)
  setLoadAction(function(ns) assign("myCount", 0, envir = ns), "setCount")
}
}
\keyword{ package }

