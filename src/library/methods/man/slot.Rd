% File src/library/methods/man/slot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Copyright 2005-2010 The R Foundation
% Distributed under GPL 2 or later

\name{slot}
\alias{slot}
\alias{.hasSlot}
\alias{slot<-}
\alias{slotNames}
\alias{.slotNames}
\alias{getSlots}
\title{The Slots in an Object from a Formal Class}
\description{
  These functions return or set information about the individual slots
  in an object.
}
\usage{
\special{object@name}% @ really is in base.
\special{object@name <- value}% @<- really is in base.

slot(object, name)
slot(object, name, check = TRUE) <- value
.hasSlot(object, name)% not saying more; prefer is(), extends(), etc!

slotNames(x)
getSlots(x)
}
%% .slotNames(x)
%%
%% NOTA BENE: We shouldn't advertize .slotNames() --
%% - - - - -  rather  slotNames() should be changed {and getSlots() too}
%%   such that it doesn't special-case class representations!
%%
\arguments{
  \item{object}{An object from a formally defined class.}
  \item{name}{The name of the slot. The operator
    takes a fixed name, which can be unquoted if it is syntactically a
    name in the language.  A slot name can be any non-empty string, but
    if the name is not made up of letters, numbers, and \code{.}, it
    needs to be quoted (by backticks or single or double quotes).

    In the case of the \code{slot} function, \code{name} can be any
    expression that evaluates to a valid slot in the class definition.
    Generally, the only reason to use the functional form rather than
    the simpler operator is \emph{because} the slot name has to be computed.
  }
  \item{value}{A new value for the named slot.  The value must be
    valid for this slot in this object's class.}
  \item{check}{In the replacement version of \code{slot}, a flag.  If
    \code{TRUE}, check the assigned value for validity
    as the value of this slot.  User's coded should not set this to
    \code{FALSE} in normal use, since the resulting object can be invalid.
  }
  \item{x}{either the name of a class (as character string), or a class
    definition.  If given an argument that is neither a character string
    nor a class definition, \code{slotNames} (only) uses \code{class(x)}
    instead.}
}
\value{
  The \code{"@"} operator and the \code{slot} function extract or
  replace the formally defined slots for the object.

  Functions \code{slotNames} and \code{getSlots} return respectively the
  names of the slots and the classes associated with the slots in the
  specified class definition.  Except for its extended interpretation of
  \code{x} (above), \code{slotNames(x)} is just \code{names(getSlots(x))}.
}
\details{
  The definition of the class specifies all slots directly and
  indirectly defined for that class.  Each slot has a name and an
  associated class.  Extracting a slot returns an object from that
  class.  Setting a slot first coerces the value to the specified slot
  and then stores it.

  Unlike general attributes, slots are not partially matched, and asking
  for (or trying to set) a slot with an invalid name for that class
  generates an error.

  The \code{\link{@}} extraction operator and \code{slot}
  function themselves do no checking against the class definition,
  simply matching the name in the object itself.
  The replacement forms do check (except for \code{slot} in the case
  \code{check=FALSE}).  So long as slots are set without cheating, the
  extracted slots will be valid.

  Be aware that there are two ways to cheat, both to be avoided but
  with no guarantees.  The obvious way is to assign a slot with
  \code{check=FALSE}.  Also, slots in \R are implemented as
  attributes, for the sake of some back compatibility.  The current
  implementation does not prevent attributes being assigned, via
  \code{\link{attr<-}}, and such assignments are not checked for
  legitimate slot names.

  Note that the \code{"@"} operators for extraction and replacement are
  primitive and actually reside in the \pkg{base} package.

  The replacement versions of  \code{"@"} and \code{slot()} differ in
  the computations done to coerce the right side of the assignment to
  the declared class of the slot.  Both verify that the value provided
  is from a subclass of the declared slot class.  The  \code{slot()}
  version will go on to call the coerce method if there is one, in
  effect doing the computation \code{as(value, slotClass, strict =
    FALSE)}. The  \code{"@"} version just verifies the relation,
  leaving any coerce to be done later (e.g., when a relevant method is
  dispatched).

  In most uses the result is equivalent, and the  \code{"@"} version
  saves an extra function call, but if empirical evidence shows that a
  conversion is needed, either call \code{as()} before the replacement
  or use the replacement version of \code{slot()}.
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
  \code{\link{@}},
  \code{\link{Classes_Details}},
  \code{\link{Methods_Details}},
  \code{\link{getClass}},
  \code{\link{names}}.
}
\examples{
\dontshow{if(isClass("track")) removeClass("track")}

setClass("track", slots = c(x="numeric", y="numeric"))
myTrack <- new("track", x = -4:4, y = exp(-4:4))
slot(myTrack, "x")
slot(myTrack, "y") <- log(slot(myTrack, "y"))
utils::str(myTrack)

getSlots("track") # or
getSlots(getClass("track"))
slotNames(class(myTrack)) # is the same as
slotNames(myTrack)

\dontshow{removeClass("track")##  should not be needed... see ./setClass.Rd}
}
\keyword{programming}
\keyword{classes}
