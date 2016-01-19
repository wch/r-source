% File src/library/methods/man/S3Part.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2016 R Core Team
% Distributed under GPL 2 or later

\name{S3Part}
\title{ S3-style Objects and S4-class Objects}
\alias{S3Part}
\alias{S3Part<-}
\alias{S3Class}
\alias{S3Class<-}
\alias{isXS3Class}
\alias{slotsFromS3}
\alias{S4}
\alias{S3}
\alias{coerce,ANY,S3-method}
\alias{coerce,oldClass,S3-method}
\alias{coerce,ANY,S4-method}
\alias{S3-class}
\description{
  Old-style (S3) classes may be registered as S4 classes (by calling
  \code{\link{setOldClass}}), and many have been.  These classes can
  then be contained in (that is, superclasses of) regular S4 classes,
  allowing formal methods and slots to be added to the S3 behavior.  The
  function \code{S3Part} extracts or replaces the S3 part of such an object.
  \code{S3Class} extracts or
  replaces the S3-style class.  \code{S3Class} also applies to object
  from an S4 class with \code{S3methods=TRUE} in the call to \code{\link{setClass}}.

  See the details below.  Also discussed are S3 <-> S4 coercion; see the
  section \dQuote{S3 and S4 objects}
}
\usage{
S3Part(object, strictS3 = FALSE, S3Class)

S3Part(object, strictS3 = FALSE, needClass = ) <- value

S3Class(object)

S3Class(object) <-  value

isXS3Class(classDef)

slotsFromS3(object)
}
\arguments{
  \item{object}{an object from some class that extends a registered
    S3 class, usually because the class has as
    one of its superclasses an S3 class registered by a call to
    \code{\link{setOldClass}}, or from a class that extends a basic
    vector, matrix or array object type.  See the details.

    For most of the functions, an S3 object can also be supplied,
    with the interpretation that it is its own S3 part.
  }
  \item{strictS3}{If \code{TRUE}, the value returned by
    \code{S3Part} will be an S3 object, with all the S4 slots
    removed.  Otherwise, an S4 object will always
    be returned; for example, from the S4 class created by
    \code{\link{setOldClass}} as a proxy for an S3 class, rather than
    the underlying S3 object.
  }
  \item{S3Class}{the \code{\link{character}} vector to be stored as the
    S3 class slot in the object.  Usually, and by default, retains
    the slot from \code{object}.
  }

  \item{needClass}{Require that the replacement value be this class or a
    subclass of it.}

  \item{value}{For \code{S3Part<-}, the replacement value for the
    S3 part of the object.  This does \emph{not} need to be an S4
    object; in fact, the usual way to create objects from these
    classes is by giving an S3 object of the right class as an
    argument to \code{\link{new}}.

    For \code{S3Class<-}, the character vector that will be used as
    a proxy for \code{class(x)} in S3 method dispatch.  This
    replacement function can be used to control S3 per-object method
    selection.
  }

  \item{classDef}{a class definition object, as returned by
    \code{\link{getClass}}.}
}
\details{
  Classes that register S3 classes by a call to
  \code{\link{setOldClass}} have slot \code{".S3Class"} to hold
  the corresponding S3 vector of class strings.
  The prototype of such
  a class has the value for this slot determined  by the argument to
  \code{\link{setOldClass}}.
  Other S4 classes will have the same slot if the argument
  \code{S3methods = TRUE} is supplied to \code{\link{setClass}}; in
  this case, the slot is set to the S4 inheritance of the class.

  New S4 classes that extend (contain) such
  classes also have the same slot, and by default the prototype has
  the value determined  by the \code{contains=} argument to
  \code{\link{setClass}}.
  Individual objects from the S4 class may
  have
  an S3 class corresponding to the value in the prototype or to an
  (S3) subclass of that value.  See the examples below.

  \code{S3Part()} with \code{strictS3 = TRUE} constructs the underlying
  S3 object by eliminating
  all the formally defined slots and turning off the S4 bit of the
  object.  With \code{strictS3 = FALSE} the object returned is from
  the corresponding S4 class.  For consistency and generality,
  \code{S3Part()} works also for classes that extend the basic vector,
  matrix and array classes.  Since \R is somewhat arbitrary about what
  it treats as an S3 class (\code{"ts"} is, but \code{"matrix"} is
  not), \code{S3Part()} tries to return an S3 (that is, non-S4) object
  whenever the S4 class has a suitable superclass, of either S3 or
  basic object type.

  One general application that relies on this generality is to use
  \code{S3Part()} to get a superclass object that is guaranteed not to
  be an S4 object.  If you are calling some function that checks for
  S4 objects, you need to be careful not to end up in a closed loop
  (\code{fooS4} calls \code{fooS3}, which checks for an S4 object and
  calls \code{fooS4} again, maybe indirectly).  Using \code{S3Part()}
  with \code{strictS3 = TRUE} is a mechanism to avoid such loops.


  Because the contents of S3 class objects have no definition or
  guarantee, the computations involving S3 parts  do
  \emph{not} check for slot validity.  Slots are implemented
  internally in \R as attributes, which are copied when present in the
  S3 part.  Grave problems can occur if an S4 class
  extending an S3 class uses the name of an S3 attribute as the name
  of an S4 slot, and S3 code sets the attribute to an object from an
  invalid class according to the S4 definition.

  Frequently, \code{S3Part} can and should be avoided by simply
  coercing objects to the desired class; methods are automatically
  defined to deal correctly with the slots when \code{\link{as}} is
  called to extract or replace superclass objects.

  The function \code{slotsFromS3()} is a generic function used
  internally to access the slots associated with the S3 part of the
  object.  Methods for this function are created automatically when
  \code{\link{setOldClass}} is called with the \code{S4Class}
  argument.  Usually, there is only one S3 slot, containing the S3
  class, but the \code{S4Class} argument may provide additional slots,
  in the case that the S3 class has some guaranteed attributes that
  can be used as formal S4 slots.  See the corresponding section in
  the documentation of \code{\link{setOldClass}}.
}
\value{

\code{S3Part}:  Returns or sets  the S3 information
  (and possibly some S4 slots as well, depending on arguments
  \code{S3Class} and \code{keepSlots}).  See the discussion of
  argument \code{strict} above.  If it is \code{TRUE} the value
  returned is an S3 object.


\code{S3Class}:  Returns or sets the character vector of S3 class(es) stored in
  the object, if the class has the corresponding \code{.S3Class} slot.
  Currently, the function defaults to \code{\link{class}} otherwise.

\code{isXS3Class}: Returns \code{TRUE} or \code{FALSE} according
    to whether the class defined by \code{ClassDef}
    extends S3 classes (specifically, whether it has the slot for
    holding the S3 class).

\code{slotsFromS3}: returns a list of the relevant slot classes, or an
empty list for any other object.
}

\section{S3 and S4 Objects: Conversion Mechanisms}{

  Objects in \R have an internal bit that indicates whether or not to
  treat the object as coming from an S4 class.  This bit is tested by
  \code{\link{isS4}} and can be set on or off by \code{\link{asS4}}.
  The latter function, however, does no checking or interpretation;
  you should only use it if you are very certain every detail has been
  handled correctly.

  As a friendlier alternative, methods have been defined for coercing
  to the virtual classes \code{"S3"} and \code{"S4"}.  The expressions
  \code{as(object, "S3")}  and \code{as(object, "S4")}  return S3
  and S4 objects, respectively.  In addition, they attempt
  to do conversions in a valid way, and also check validity when
  coercing to S4.

  The expression \code{as(object, "S3")} can be used in two ways.  For
  objects from one of the registered S3 classes, the expression will
  ensure that the class attribute is the full multi-string S3 class
  implied by \code{class(object)}.  If the registered class has known
  attribute/slots, these will also be provided.

  Another use of  \code{as(object, "S3")}  is to take an S4 object and
  turn it into an S3 object with corresponding attributes.  This is
  only meaningful with S4 classes that have a data part.  If you want
  to operate on the object without invoking S4 methods, this
  conversion is usually the safest way.

  The expression  \code{as(object, "S4")} will use the attributes in
  the object to create an object from the S4 definition of
  \code{class(object)}. This is a general mechanism to create
  partially defined version of S4 objects via S3 computations  (not
  much different from invoking \code{\link{new}} with corresponding
  arguments, but usable in this form even if the S4 object has an
  initialize method with different arguments).
}

\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version).

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\seealso{  \code{\link{setOldClass}} }
\examples{
## two examples extending S3 class "lm", class "xlm" directly
## and "ylm" indirectly
setClass("xlm", representation(eps = "numeric"), contains = "lm")
setClass("ylm", representation(header = "character"), contains = "xlm")
\dontshow{
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2,10,20, labels=c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
}
## lm.D9 is as computed in the example for stats::lm
y1 <- new("ylm", lm.D9, header = "test", eps = .1)
xx <- new("xlm", lm.D9, eps =.1)
y2 <- new("ylm", xx, header = "test")
stopifnot(inherits(y2, "lm"))
stopifnot(identical(y1, y2))
stopifnot(identical(S3Part(y1, strict = TRUE), lm.D9))

## note the these classes can insert an S3 subclass of "lm" as the S3 part:
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData) # S3 class: c("mlm", "lm")
ym1 <- new("ylm", myLm, header = "Example", eps = 0.)

##similar classes to "xlm" and "ylm", but extending S3 class c("mlm", "lm")
setClass("xmm", representation(eps = "numeric"), contains = "mlm")
setClass("ymm", representation(header="character"), contains = "xmm")

ym2 <- new("ymm", myLm, header = "Example2", eps = .001)

# but for class "ymm", an S3 part of class "lm" is an error:
try(new("ymm", lm.D9, header = "Example2", eps = .001))
\dontshow{tools::assertError(
    new("ymm", lm.D9, header = "Example2", eps = .001))}

setClass("dataFrameD", representation(date = "Date"),
         contains = "data.frame")
myDD <- new("dataFrameD", myData, date = Sys.Date())

## S3Part() applied to classes with a data part (.Data slot)

setClass("NumX", contains="numeric", representation(id="character"))
nn <- new("NumX", 1:10, id="test")
stopifnot(identical(1:10, S3Part(nn, strict = TRUE)))

m1 <- cbind(group, weight)
setClass("MatX", contains = "matrix", representation(date = "Date"))
mx1 <- new("MatX", m1, date = Sys.Date())
stopifnot(identical(m1, S3Part(mx1, strict = TRUE)))

\dontshow{
for(cl in c("ylm", "xlm", "ymm", "xmm", "dataFrameD", "NumX", "MatX"))
    removeClass(cl)
}
}
\keyword{ programming }
\keyword{ classes }
