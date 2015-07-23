% File src/library/base/man/trace.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{trace}
\title{Interactive Tracing and Debugging of Calls to a Function or Method}
\alias{trace}
\alias{untrace}
\alias{tracingState}
\alias{.doTrace}
\alias{returnValue}
\description{
  A call to \code{trace} allows you to insert debugging code (e.g., a
  call to \code{\link{browser}} or \code{\link{recover}}) at chosen
  places in any function.  A call to \code{untrace} cancels the tracing.
  Specified methods can be traced the same way, without tracing all
  calls to the function.  Trace code can be any \R expression.  Tracing
  can be temporarily turned on or off globally by calling \code{tracingState}.
}
\usage{
trace(what, tracer, exit, at, print, signature,
      where = topenv(parent.frame()), edit = FALSE)
untrace(what, signature = NULL, where = topenv(parent.frame()))

tracingState(on = NULL)
.doTrace(expr, msg)
returnValue(default = NULL)
}
\arguments{
  \item{what}{The name (quoted or not) of a function to be traced or
    untraced.  For \code{untrace} or for \code{trace} with more than one
    argument, more than one name can be given in the quoted form, and
    the same action will be applied to each one.}
  \item{tracer}{Either a function or an unevaluated expression.  The
    function will be called or the expression will be evaluated either
    at the beginning of the call, or before those steps in the call
    specified by the argument \code{at}.
    See the details section.}
  \item{exit}{ Either a function or an unevaluated expression.  The
    function will be called or the expression will be evaluated on
    exiting the function.
    See the details section.}
  \item{at}{optional numeric vector or list.  If supplied, \code{tracer}
    will be called just before the corresponding step in the body of the
    function.
    See the details section. }
  \item{print}{If \code{TRUE} (as per default), a descriptive line is
    printed before any trace expression is evaluated.}
  \item{signature}{ If this argument is supplied, it should be a
    signature for a method for function \code{what}.  In this case, the
    method, and \emph{not} the function itself, is traced.}
  \item{edit}{ For complicated tracing, such as tracing within a loop
    inside the function, you will need to insert the desired calls by
    editing the body of the function.  If so, supply the \code{edit}
    argument either as \code{TRUE}, or as the name of the editor you
    want to use.  Then \code{trace()} will call \code{\link{edit}} and
    use the version of the function after you edit it.  See the details
    section for additional information.
    }
  \item{where}{where to look for the function to be
    traced; by default, the top-level environment of the call to
    \code{trace}.

    An important use of this argument is to trace a function when it
  is called from a package with a namespace.  The current namespace mechanism
  imports the functions to be called (with the exception of functions
  in the base package).  The functions being called are \emph{not} the
  same objects seen from the top-level (in general, the imported
  packages may not even be attached).  Therefore, you must ensure that
  the correct versions are being traced.  The way to do this is to set
  argument \code{where} to a function in the namespace.  The tracing
  computations will then start looking in the environment of that
  function (which will be the namespace of the corresponding package).
  (Yes, it's subtle, but the semantics here are central to how
  namespaces work in R.)
  }
  \item{on}{logical; a call to  the support function \code{tracingState} returns \code{TRUE}
    if tracing is globally turned on, \code{FALSE} otherwise.  An
    argument of one or the other of those values sets the state.  If the
    tracing state is \code{FALSE}, none of the trace actions will
    actually occur (used, for example, by debugging functions to shut
    off tracing during debugging).}
  \item{expr, msg}{arguments to the support function \code{.doTrace}, calls to
    which are inserted into the modified function or method:
    \code{expr} is the tracing action (such as a call to
    \code{browser()}, and \code{msg} is a string identifying the
    place where the trace action occurs.
  }
  \item{default}{If \code{returnValue} finds no return value (e.g.
    a function exited because of an error, not a normal exit), it
    will return \code{default} instead.
  }
}
\details{
  The \code{trace} function operates by constructing a revised version
  of the function (or of the method, if \code{signature} is supplied),
  and assigning the new object back where the original was found.
  If only the \code{what} argument is given, a line of trace printing is
  produced for each call to the function (back compatible with the
  earlier version of \code{trace}).

  The object constructed by \code{trace} is from a class that extends
  \code{"function"} and which contains the original, untraced version.
  A call to \code{untrace} re-assigns this version.

  If the argument \code{tracer} or \code{exit} is the name of a
  function, the tracing expression will be a call to that function, with
  no arguments.  This is the easiest and most common case, with the
  functions \code{\link{browser}} and \code{\link{recover}} the
  likeliest candidates; the former browses in the frame of the function
  being traced, and the latter allows browsing in any of the currently
  active calls.

  The \code{tracer} or \code{exit} argument can also be an unevaluated
  expression (such as returned by a call to \code{\link{quote}} or
  \code{\link{substitute}}).  This expression itself is inserted in the
  traced function, so it will typically involve arguments or local
  objects in the traced function.  An expression of this form is useful
  if you only want to interact when certain conditions apply (and in
  this case you probably want to supply \code{print = FALSE} in the call
  to \code{trace} also).

  When the \code{at} argument is supplied, it can be a vector of
  integers referring to the substeps of the body of the function (this
  only works if the body of the function is enclosed in \code{{ ...}}.  In
  this case \code{tracer} is \emph{not} called on entry, but instead
  just before evaluating each of the steps listed in \code{at}.  (Hint:
  you don't want to try to count the steps in the printed version of a
  function; instead, look at \code{as.list(body(f))} to get the numbers
  associated with the steps in function \code{f}.)

  The \code{at} argument can also be a list of integer vectors.  In
  this case, each vector refers to a step nested within another step of
  the function.  For example, \code{at = list(c(3,4))}
  will call the tracer just before the fourth step of the third step
  of the function.  See the example below.

  Using \code{\link{setBreakpoint}} (from package \pkg{utils}) may be an
  alternative, calling \code{trace(...., at, ...)}.
  
  The \code{exit} argument is called during \code{\link{on.exit}}
  processing.  In an \code{on.exit} expression, the experimental \code{returnValue()}
  function may be called to obtain the value about to be returned by
  the function. Calling this function in other circumstances will give
  undefined results.  

  An intrinsic limitation in the \code{exit} argument is that it won't
  work if the function itself uses \code{on.exit} with \code{add=
  FALSE} (the default), since the existing calls will override the one
  supplied by \code{trace}.

  Tracing does not nest.  Any call to \code{trace} replaces previously
  traced versions of that function or method (except for edited
  versions as discussed below), and \code{untrace} always
  restores an untraced version.  (Allowing nested tracing has too many
  potentials for confusion and for accidentally leaving traced versions
  behind.)

  When the \code{edit} argument is used repeatedly with no call to
  \code{untrace} on the same function or method in between, the
  previously edited version is retained.  If you want to throw away
  all the previous tracing and then edit, call \code{untrace} before the next
  call to \code{trace}.  Editing may be combined with automatic
  tracing; just supply the other arguments such as \code{tracer}, and
  the \code{edit} argument as well.  The \code{edit = TRUE} argument
  uses the default editor (see \code{\link{edit}}).

  Tracing primitive functions (builtins and specials) from the base
  package works, but only by a special mechanism and not very
  informatively.  Tracing a primitive causes the primitive to be
  replaced by a function with argument \dots (only).  You can get a bit
  of information out, but not much.  A warning message is issued when
  \code{trace} is used on a primitive.

  The practice of saving the traced version of the function back where
  the function came from means that tracing carries over from one
  session to another, \emph{if} the traced function is saved in the
  session image.  (In the next session, \code{untrace} will remove the
  tracing.)  On the other hand, functions that were in a package, not in
  the global environment, are not saved in the image, so tracing expires
  with the session for such functions.

  Tracing a method is basically just like tracing a function, with the
  exception that the traced version is stored by a call to
  \code{\link{setMethod}} rather than by direct assignment, and so is
  the untraced version after a call to \code{untrace}.

  The version of \code{trace} described here is largely compatible with
  the version in S-Plus, although the two work by entirely different
  mechanisms.  The S-Plus \code{trace} uses the session frame, with the
  result that tracing never carries over from one session to another (\R
  does not have a session frame).  Another relevant distinction has
  nothing directly to do with \code{trace}:  The browser in S-Plus
  allows changes to be made to the frame being browsed, and the changes
  will persist after exiting the browser.  The \R browser allows changes,
  but they disappear when the browser exits.  This may be relevant in
  that the S-Plus version allows you to experiment with code changes
  interactively, but the \R version does not.  (A future revision may
  include a \sQuote{destructive} browser for \R.)
}
\note{
  The version of function tracing that includes any of the arguments
  except for the function name requires the \pkg{methods} package
  (because it uses special classes of objects to store and restore
  versions of the traced functions).

  If methods dispatch is not currently on, \code{trace} will load the
  methods namespace, but will not put the methods package on the search
  list.
}
\value{
  In the simple version (just the first argument), \code{trace} returns
  an invisible \code{NULL}.
  Otherwise, the traced function(s) name(s).  The relevant consequence is the
  assignment that takes place.
  
  \code{untrace} returns the function name invisibly.
  
  \code{tracingState} returns the current global tracing state, and possibly
  changes it.
  
  When called during \code{on.exit} processing, \code{returnValue} returns
  the value about to be returned by the exiting function.  Behaviour in 
  other circumstances is undefined.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{browser}} and \code{\link{recover}}, the likeliest
  tracing functions;
  also, \code{\link{quote}} and \code{\link{substitute}} for
  constructing general expressions.
}
\examples{
require(graphics)

##  Very simple use
trace(sum)
hist(stats::rnorm(100)) # shows about 3-4 calls to sum()
untrace(sum)

if(.isMethodsDispatchOn()) { # non-simple use needs 'methods' package

f <- function(x, y) {
    y <- pmax(y, 0.001)
    if (x > 0) x ^ y else stop("x must be positive")
}

## arrange to call the browser on entering and exiting
## function f
trace("f", quote(browser(skipCalls = 4)),
      exit = quote(browser(skipCalls = 4)))

## instead, conditionally assign some data, and then browse
## on exit, but only then.  Don't bother me otherwise

trace("f", quote(if(any(y < 0)) yOrig <- y),
      exit = quote(if(exists("yOrig")) browser(skipCalls = 4)),
      print = FALSE)

## Enter the browser just before stop() is called.  First, find
## the step numbers

as.list(body(f))
as.list(body(f)[[3]])

## Now call the browser there

trace("f", quote(browser(skipCalls = 4)), at = list(c(3,4)))

## trace a utility function, with recover so we
## can browse in the calling functions as well.

trace("as.matrix", recover)

## turn off the tracing

untrace(c("f", "as.matrix"))

\dontrun{
## trace calls to the function lm() that come from
## the nlme package.
## (The function nlme is in that package, and the package
## has a namespace, so the where= argument must be used
## to get the right version of lm)

trace(lm, exit = recover, where = nlme)
}
}
}
\keyword{programming}
\keyword{debugging}
