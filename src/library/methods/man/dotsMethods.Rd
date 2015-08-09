% File src/library/methods/man/dotsMethods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{dotsMethods}
\alias{dotsMethods}
\title{The Use of \code{...} in Method Signatures}
\description{
  The \dQuote{\dots} argument in \R functions is treated specially, in that it
  matches zero, one or more actual arguments (and so, objects).  A
  mechanism has been added to \R to allow \dQuote{\dots} as the signature of a
  generic function.  Methods defined for such functions will be
  selected and called when \emph{all}  the arguments matching \dQuote{\dots}
  are from the specified class or from some subclass of that class.
}

\section{Using "..." in a Signature}{
  Beginning with version 2.8.0 of \R, S4 methods can be dispatched
  (selected and called) corresponding to the special argument \dQuote{\dots}.
  Currently, \dQuote{\dots} cannot be mixed with other formal arguments:
  either the signature of the generic function is \dQuote{\dots} only, or it
  does not contain \dQuote{\dots}.  (This restriction may be lifted in a future
  version.)

  Given a suitable generic function, methods are specified in the
  usual way by a call to \code{\link{setMethod}}.  The method
  definition must be written expecting all the arguments corresponding
  to \dQuote{\dots} to be from the class specified in the method's signature,
  or from a class that extends that class (i.e., a subclass of that
  class).

  Typically the methods will pass \dQuote{\dots} down to another function or
  will create a list of the arguments and iterate over that.  See the
  examples below.

  When you have a computation that is suitable for more than one existing
  class, a convenient approach may be to define a union of these
  classes by a call to \code{\link{setClassUnion}}. See the example
  below.

}
\section{Method Selection and Dispatch for "..."}{
  See \link{Methods} for a general discussion.  The following assumes
  you have read the \dQuote{Method Selection and Dispatch} section of
  that documentation.

  A method selecting on \dQuote{\dots} is specified by a single class in the
  call to \code{\link{setMethod}}.  If all the actual arguments
  corresponding to \dQuote{\dots} have this class, the corresponding method is
  selected directly.

  Otherwise, the class of each argument and that class' superclasses are
  computed, beginning with the first \dQuote{\dots} argument.  For the first
  argument, eligible methods are those for any of the classes.   For
  each succeeding argument that introduces a class not considered previously, the eligible methods are further
  restricted to those matching the argument's class or
  superclasses. If no further eligible classes exist, the iteration
  breaks out and the default method, if any, is selected.

  At the end of the iteration, one or more methods may be eligible.
  If more than one, the selection looks for the method with the least
  distance to the actual arguments.  For each argument, any inherited
  method corresponds to a distance, available from the \code{contains}
  slot of the class definition.  Since the same class can arise for
  more than one argument, there may be several distances associated
  with it.  Combining them is inevitably arbitrary:  the current
  computation uses the minimum distance.  Thus, for example, if a
  method matched one argument directly, one as  first generation
  superclass and another as a second generation superclass, the
  distances are 0, 1 and 2.  The current selection computation would
  use distance 0 for this
  method.  In particular, this selection criterion tends to use a method that
  matches exactly one or more of the arguments' class.

  As with ordinary method selection, there may be multiple methods
  with the same distance.  A warning  message is issued and one of the
  methods is chosen (the first encountered, which in this case is
  rather arbitrary).

  Notice that, while the computation examines all arguments, the
  essential cost of dispatch goes up with the number of
  \emph{distinct} classes among the arguments, likely to be much
  smaller than the number of arguments when the latter is large.

}

\section{Implementation Details}{
  Methods dispatching on \dQuote{\dots} were introduced in version 2.8.0 of
  \R.  The initial implementation of the corresponding selection and
  dispatch is in an R function, for flexibility while the new
  mechanism is being studied.  In this implementation, a local version
  of \code{setGeneric} is inserted in the generic function's
  environment.  The local version selects a method according to the
  criteria above and calls that method, from the environment of the
  generic function.  This is slightly different from the action taken
  by the C implementation when \dQuote{\dots} is not involved.  Aside from the
  extra computing time required, the method is evaluated in a true
  function call, as opposed to the special context constructed by the
  C version (which cannot be exactly replicated in R code.)  However,
  situations in which different computational results would
  be obtained have not been encountered so far, and seem very
  unlikely.

  Methods dispatching on arguments other than \dQuote{\dots} are \emph{cached} by storing
  the inherited method in the table of all methods, where it will be
  found on the next selection with the same combination of classes
  in the actual arguments (but not used for inheritance searches).
  Methods based on \dQuote{\dots} are also cached, but not found quite
  as immediately.  As noted, the selected method depends only on the
  set of classes that occur in the \dQuote{\dots} arguments.  Each of
  these classes can appear one or more times, so many combinations of
  actual argument classes will give rise to the same effective
  signature.  The selection computation first computes and sorts the
  distinct classes encountered.  This gives a label that will be
  cached in the table of all methods, avoiding any further search for
  inherited classes after the first occurrence.  A call to
  \code{\link{showMethods}} will expose such inherited methods.

  The intention is that the \dQuote{\dots} features will be added to the
  standard C code when enough experience with them has been obtained.
  It is possible that at the same time, combinations of \dQuote{\dots} with
  other arguments in signatures may be supported.
}

\examples{
cc <- function(...)c(...)

setGeneric("cc")

setMethod("cc", "character", function(...)paste(...))

setClassUnion("Number", c("numeric", "complex"))

setMethod("cc", "Number", function(...) sum(...))

setClass("cdate", contains = "character", representation(date = "Date"))

setClass("vdate", contains = "vector", representation(date = "Date"))

cd1 <- new("cdate", "abcdef", date = Sys.Date())

cd2 <- new("vdate", "abcdef", date = Sys.Date())

stopifnot(identical(cc(letters, character(), cd1),
           paste(letters, character(), cd1))) # the "character" method

stopifnot(identical(cc(letters, character(), cd2),
                    c(letters, character(), cd2)))
# the default, because "vdate" doesn't extend "character"

stopifnot(identical(cc(1:10, 1+1i), sum(1:10, 1+1i))) # the "Number" method

stopifnot(identical(cc(1:10, 1+1i, TRUE), c(1:10, 1+1i, TRUE))) # the default

stopifnot(identical(cc(), c())) # no arguments implies the default method

setGeneric("numMax", function(...)standardGeneric("numMax"))

setMethod("numMax", "numeric", function(...)max(...))
# won't work for complex data
setMethod("numMax", "Number", function(...) paste(...))
# should not be selected w/o complex args

stopifnot(identical(numMax(1:10, pi, 1+1i), paste(1:10, pi, 1+1i)))
stopifnot(identical(numMax(1:10, pi, 1), max(1:10, pi, 1)))

try(numMax(1:10, pi, TRUE)) # should be an error:  no default method

## A generic version of paste(), dispatching on the "..." argument:
setGeneric("paste", signature = "...")

setMethod("paste", "Number", function(..., sep, collapse) c(...))

stopifnot(identical(paste(1:10, pi, 1), c(1:10, pi, 1)))

\dontshow{
for(gen in c("numMax", "cc", "paste")) removeGeneric(gen)
for(cl in c("Number", "vdate", "cdate")) removeClass(cl)
}
}

\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{
For the general discussion of methods, see  \link{Methods} and links
from there.
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
