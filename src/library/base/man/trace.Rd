\name{trace}
\alias{trace}
\alias{untrace}
\alias{tracingState}
%% internal
\alias{.primTrace}
\alias{.primUntrace}
\title{ Interactive Tracing and Debugging of Calls to a Function or
  Method}
\description{
  A call to \code{trace} allows you to insert debugging code (e.g., a
  call to \code{\link{browser}} or \code{\link{recover}}) at chosen
  places in any function.  A call to \code{untrace} cancels the tracing.
  Specified methods can be traced the same way, without tracing all
  calls to the function.  Trace code can be any \R expression. Tracing
  can be temporarily turned on or off globally by calling \code{tracingState}.
}
\usage{
trace(what, tracer, exit, at, print = TRUE, signature = NULL)
untrace(what, signature = NULL)

tracingState(on)
}
\arguments{ 
  \item{what}{ The name (quoted or not) of a function to be traced or
    untraced.  More than one name can be given in the quoted form, and
    the same action will be applied to each one. }
  \item{tracer}{Either a function or an unevaluated expression.  The
    function will be called or the expression will be evaluated either
    at the beginning of the call, or before those steps in the call
    specified by the argument \code{at}.
    See the details section.}
  \item{exit}{ Either a function or an unevaluated expression.  The
    function will be called or the expression will be evaluated on
    exiting the function.
    See the details section. }
  \item{at}{ An optional numeric vector.  If supplied, \code{tracer}
    will be called just before the corresponding step in the body of the
    function.
    See the details section. }
  \item{print}{ If \code{TRUE}, a descriptive line is printed before any
    trace expression is evaluated.}
  \item{signature}{ If this argument is supplied, it should be a
    signature for a method for function \code{what}.  In this case, the
    method, and \emph{not} the function itself, is traced. }

 \item{on}{A call to \code{tracingState} returns \code{TRUE} if
     tracing is globally turned on, \code{FALSE} otherwise.  An
     argument of one or the other of those values sets the state.  If
     the tracing state is \code{FALSE}, none of the trace actions will
     actually occur (used, for example, by debugging functions to shut
     off tracing during debugging).
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
  this case you probably want to supply \code{print=FALSE} in the call
  to \code{trace} also).

  When the \code{at} argument is supplied, it should be a vector of
  integers referring to the substeps of the body of the function (this
  only works if the body of the function is enclosed in \code{{ ...}}.  In
  this case \code{tracer} is \emph{not} called on entry, but instead
  just before evauating each of the steps listed in \code{at}.  (Hint:
  you don't want to try to count the steps in the printed version of a
  function; instead, look at \code{as.list(body(f))} to get the numbers
  associated with the steps in function \code{f}.)

  An intrinsic limitation in the \code{exit} argument is that it won't
  work if the function itself uses \code{on.exit}, since the existing
  calls will override the one supplied by \code{trace}.

  Tracing does not nest.  Any call to \code{trace} replaces previously
  traced versions of that function or method, and \code{untrace} always
  restores an untraced version.  (Allowing nested tracing has too many
  potentials for confusion and for accidentally leaving traced versions
  behind.)

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
  \code{\link[methods]{setMethod}} rather than by direct assignment, and so is
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
  include a ``destructive'' browser for \R.)
}
\note{  The version of function tracing that includes any of the
  arguments except for the function name requires the methods package
  (because it uses special classes of objects to store and restore
  versions of the traced functions).

  In rare cases, this may change some behavior.  In particular, although
  the methods package
  generally supports older styles of classes and methods, it is
  stricter in treating class attributes.  Just occasionally, older code may stop
  on an error with the methods package attached, but not otherwise.
  }
\value{
  The traced function(s) name(s).  The relevant consequence is the
  assignment that takes place.
}
\seealso{
  \code{\link{browser}} and \code{\link{recover}}, the likeliest
  tracing functions;
  also, \code{\link{quote}} and \code{\link{substitute}} for
  constructing general expressions.
}
\examples{

\testonly{ hasMethods <- .isMethodsDispatchOn()}

f <- function(x, y) {
    y <- pmax(y, .001)
    x ^ y
}

## arrange to call the browser on entering and exiting
## function f
trace("f", browser, exit = browser)

## instead, conditionally assign some data, and then browse
## on exit, but only then.  Don't bother me otherwise

trace("f", quote(if(any(y < 0)) yOrig <- y),
      exit = quote(if(exists("yOrig")) browser()),
      print = FALSE)

## trace a utility function, with recover so we
## can browse in the calling functions as well.

trace("as.matrix", recover)

## turn off the tracing

untrace(c("f", "as.matrix"))

\testonly{


f <- function(x, y) { c(x,y)}

xy <- 0

trace(f, quote(x <- c(1, x)), exit = quote(xy <<- x), print = FALSE)

fxy <- f(2,3)

stopifnot(identical(fxy, c(1,2,3)))
stopifnot(identical(xy, c(1,2)))

untrace(f)

## a generic and its methods

setGeneric("f")

setMethod("f", c("character", "character"), function(x,  y) paste(x,y))

## trace the generic
trace("f", quote(x <- c("A", x)), exit = quote(xy <<- c(x, "Z")), print = FALSE)

## should work for any method

stopifnot(identical(f(4,5), c("A",4,5)))
stopifnot(identical(xy, c("A", 4, "Z")))

stopifnot(identical(f("B", "C"), paste(c("A","B"), "C")))
stopifnot(identical(xy, c("A", "B", "Z")))

## trace a method

trace("f", sig = c("character", "character"), quote(x <- c(x, "D")),
      exit = quote(xy <<- xyy <<- c(x, "W")), print = FALSE)

stopifnot(identical(f("B", "C"), paste(c("A","B","D"), "C")))
stopifnot(identical(xy, c("A", "B", "D", "W")))
stopifnot(identical(xy, xyy))

## but the default method is unchanged

stopifnot(identical(f(4,5), c("A",4,5)))
stopifnot(identical(xy, c("A", 4, "Z")))

removeGeneric("f")

if(!hasMethods)
   detach("package:methods")

}

}
\keyword{programming}
\keyword{debugging}
