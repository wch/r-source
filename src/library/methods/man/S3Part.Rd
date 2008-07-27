\name{S3Part}
\alias{S3Part}
\alias{S3Part<-}
\alias{S3Class}
\alias{S3Class<-}
\alias{isXS3Class}
\title{ S3-style Objects Contained in S4-class Objects}
\description{
  Old-style (S3) classes may be registered as S4 classes (by calling
  \code{\link{setOldClass}}, and many have been.  These classes can
  then be contained in (that is, superclasses of) regular S4 classes, allowing formal methods
  and slots to be added to the S3 behavior.  The function
  \code{S3Part} extracts or replaces 
  the S3 part of such an object and \code{S3Class} extracts or
  replaces the S3-style class.  Most computations that need to deal with the S3
  aspects can coerce the objects automatically, without direct calls
  to these functions.
}
\usage{

S3Part(object, keepSlots = FALSE, S3Class = object@.S3Class, strict = FALSE)

S3Part(object, keepSlots = FALSE, needClass = "oldClass") <- value

S3Class(object)

S3Class(object) <-  value

isXS3Class(classDef)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{object}{ An object from some class that extends
      \code{\linkS4class{oldClass}}, usually because the class has as
      one of its superclasses an S3 class registered by a call to \code{\link{setOldClass}}. }
  \item{S3Class}{ The character vector to be stored as the S3 class
      slot in the object returned.  Usually, and by default, retains
      the slot from \code{object}. If \code{keepSlots=TRUE}, the slots
    in this class will be retained (in which case \code{S3Class} can
    be an S4 class that contains the S3 object).}
  \item{keepSlots}{ Should the computation keep any S4 slots in the
      object (or the replacement value for the replacement function).
    If \code{FALSE}, only the strictly S3 attributes will be kept or used.}
  \item{needClass}{The replacement value should be this class or a
      subclass of it.
      }

  \item{value}{The replacement value for the S3 part of the object.
      This does \emph{not} need to be an S4 object; in fact, the
      usual way to create objects from these classes is by giving an
      S3 object of the right class as an argument to
      \code{\link{new}}.
    }

    \item{strict}{  If \code{TRUE}, the value returned by
        \code{S3Part} will be an S3 object, with all the S4 slots
        removed (and in this case it is an error to specify
        \code{keepSlots=TRUE}).  Otherwise, an S4 object will always
        be returned; for example, from the S4 class created by
  \code{\link{setOldClass}} as a proxy for an S3 class, rather than
  the underlying S3 object.
        }

    \item{classDef}{ A class definition object, as returned by \code{\link{getClass}}.
        }


}
\details{
  Classes that extend S3 classes have slot \code{".S3Class"} to hold
  the corresponding S3 vector of class strings.  The prototype of such
  a class has the value determined by the \code{contains=} argument to
  \code{\link{setClass}} or by the argument to
  \code{\link{setOldClass}}. Individual objects from the class may
  have 
  an S3 class corresponding to the value in the prototype or to an
  (S3) subclass of that value.  See the examples below.

  Because the contents of S3 class objects have no definition or
  guarantee, the computations involving S3 parts  do
  \emph{not}| check for slot validity.  Slots are implemented
  internally in \R as attributes, which are copied when present in the
  S3 part.  For this reason, grave problems can occur if an S4 class
  extending an S3 class uses the name of an S3 attribute as the name
  of an S4 slot.  It's tempting to \dQuote{promote} an attribute to a
  slot, but the resulting confusion between slot and attribute is hard
  to handle.

  Generally, \code{S3Part} can and should be avoided by simply
  coercing objects to the desired class; methods are automatically
  defined to deal correctly with the slots when \code{\link{as}} is
  called to extract or replace superclass objects.
}
\value{

  \code{S3Part}:  {Returns or sets  the S3 information
  (and possibly some S4 slots as well, depending on arguments
  \code{S3Class} and \code{keepSlots}).  See the discussion of
  argument \code{strict} above.  If it is \code{TRUE} the value
  returned is an S3 object.
  }


  \code{S3Class}:  For an object from a class extending S3
      classes, returns or sets the character vector of class(es) stored in
  the object.  Note that  \code{S3Class} on any other object returns
  the value of  \code{\link{class}}.

  \code{isXS3Class}: Returns \code{TRUE} or \code{FALSE} according
    to whether the class defined by \code{ClassDef}
    extends S3 classes (specifically, whether it has the slot for
    holding the S3 class).
}

\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version).

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version.)
}
\examples{
## two examples extending S3 class "lm", class "xlm"  directly and "ylm" indirectly
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
y1 = new("ylm", lm.D9, header = "test", eps = .1)
xx = new("xlm", lm.D9, eps =.1)
y2 = new("ylm", xx, header = "test")
stopifnot(inherits(y2, "lm"))
stopifnot(identical(y1, y2))
stopifnot(identical(S3Part(y1, strict = TRUE), lm.D9))

## note the these classes can insert an S3 subclass of "lm" as the S3 part:
myData <- data.frame(time = 1:10, y = (1:10)^.5)
myLm <- lm(cbind(y, y^3)  ~ time, myData)
ym1 = new("ylm", myLm, header = "Example", eps = 0.)

##similar classes to "xlm" and "ylm", but extending S3 class c("mlm", "lm")
setClass("xmm", representation(eps = "numeric"), contains = "mlm")
setClass("ymm", representation(header="character"), contains = "xmm")

ym2 <- new("ymm", myLm, header = "Example2", eps = .001)

# but for class "ymm", an S3 part of class "lm" is an error:
try(new("ymm", lm.D9, header = "Example2", eps = .001))

setClass("dataFrameD", representation(date = "Date"), contains = "data.frame")
myDD <- new("dataFrameD", myData, date = Sys.Date())

\dontshow{

for(cl in c("ylm", "xlm", "ymm", "xmm", "dataFrameD")) removeClass(cl)

}
}
\seealso{  \code{\link{setOldClass}} }
\keyword{ programming }
\keyword{ classes }
