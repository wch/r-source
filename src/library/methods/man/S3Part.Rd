% File src/library/methods/man/S3Part.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2016 R Core Team
% Distributed under GPL 2 or later

\name{S3Part}
\title{ S4 Classes that Contain S3 Classes}
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
  A regular (S4) class may contain an S3 class, if that class has been registered (by calling
  \code{\link{setOldClass}}).  The functions described here provide
  information about contained S3 classes.  See the section \sQuote{Functions}.

  In modern \R, these functions are not
  usually needed to program with objects from the S4 class.  Standard computations work as expected, including method selection
  for both S4 and S3.  To coerce an object to its contained S3 class,
  use either of the expressions:

\code{as(object, S3Class); as(object, "S3")} 

  where
  \code{S3Class} evaluates to the name of the contained class.  These
  return slightly different objects, which in rare cases may need to
  be distinguished.  See the section \dQuote{Contained S3 Objects}.


}

\usage{
S3Part(object, strictS3 = FALSE, S3Class)

S3Class(object)

isXS3Class(classDef)

slotsFromS3(object)

## the replacement versions of the functions are not recommended
## Create a new object from the class or use the replacement version of as().


S3Part(object, strictS3 = FALSE, needClass = ) <- value

S3Class(object) <-  value

}
\arguments{
  \item{object}{an object from some class that extends a registered
    S3 class, or a basic
    vector, matrix or array object type.

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
    the slot from \code{object}, but an S3 superclass is allowed.
    }

  \item{classDef}{a class definition object, as returned by
    \code{\link{getClass}}.

\emph{The remaining arguments apply only to the replacement versions,
  which are not recommended.}
  }


  \item{needClass}{Require that the replacement value be this class or a
    subclass of it.}

  \item{value}{For \code{S3Part<-}, the replacement value for the
    S3 part of the object.

    For \code{S3Class<-}, the character vector that will be used as
    a proxy for \code{class(x)} in S3 method dispatch.
  }
}

\section{Functions}{

\code{S3Part}:  Returns an object from the S3 class that appeared
   in the \code{contains=} argument to \code{\link{setClass}}.

   If called with \code{strictS3 = TRUE}, \code{S3Part()} constructs the underlying
  S3 object by eliminating
  all the formally defined slots and turning off the S4 bit of the
  object.  With \code{strictS3 = FALSE} the object returned is from
  the corresponding S4 class.  For consistency and generality,
  \code{S3Part()} works also for classes that extend the basic vector,
  matrix and array classes.

  A call to is equivalent coercing the object to class \code{"S3"} for
  the strict case, or to whatever the specific S3 class was, for the
  non-strict case.  The \code{as()} calls are usually easier for
  readers to understand.



\code{S3Class}:  Returns the character vector of S3 class(es) stored in
  the object, if the class has the corresponding \code{.S3Class} slot.
  Currently, the function defaults to \code{\link{class}} otherwise.

\code{isXS3Class}: Returns \code{TRUE} or \code{FALSE} according
    to whether the class defined by \code{ClassDef}
    extends S3 classes (specifically, whether it has the slot for
    holding the S3 class).

\code{slotsFromS3}: returns a list of the relevant slot classes, or an
empty list for any other object.


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

\section{Contained S3 Objects}{
  Registering an S3 class defines an S4 class.  Objects from this
  class are essentially identical in content to an object from the S3
  class, except for two differences.  The value returned by
  \code{\link{class}()} will always be a single string for the S4
  object, and \code{\link{isS4}()} will return \code{TRUE} or
  \code{FALSE} in the two cases.  See the example below.  It is barely
  possible that some S3 code will not work with the S4 object; if so,
  use \code{as(x, "S3")}.

  Objects from a class that extends an S3 class will have some basic type and
  possibly some attributes.  For an S3 class that has an equivalent S4
  definition (e.g., \code{"data.frame"}), an extending S4 class will
  have a data part and slots.  For other S3 classes (e.g., \code{"lm"}) an
  object from the extending S4 class will be some sort of basic type,
  nearly always a vector type (e.g., \code{"list"} for \code{"lm"}),
  but the data part will not have a formal definition.

  Registering an S3 class by a call to
  \code{\link{setOldClass}} creates a class of the same name with a slot \code{".S3Class"} to hold
  the corresponding S3 vector of class strings.
  New S4 classes that extend such
  classes also have the same slot, set to the S3 class of the
  contained S3 \emph{object},
  which may be  an
  (S3) subclass of the registered class.
  For example, an S4 class might contain the S3 class \code{"lm"}, but
  an object from the class might contain an object from class
  \code{"mlm"}, as in the \code{"xlm"}example below.
  
  \R is somewhat arbitrary about what
  it treats as an S3 class: \code{"ts"} is, but \code{"matrix"} and \code{"array"}
  are not.
  For classes that extend
  those, assuming they contain an S3 class is incorrect and will cause some
  confusion---not usually disastrous, but the better strategy
  is to stick to the explicit \dQuote{class}.
  Thus \code{as(x, "matrix")} rather than \code{as(x, "S3")} or \code{S3Part(x)}.

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
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10, particularly Section 10.8)
}

\seealso{  \code{\link{setOldClass}} }
\examples{

## an "mlm" object, regressing two variables on two others

sepal <- as.matrix(datasets::iris[,c("Sepal.Width", "Sepal.Length")])
fit <- lm(sepal ~ Petal.Length + Petal.Width + Species, data = datasets::iris)
class(fit) # S3 class: "mlm", "lm"

## a class that contains "mlm"
myReg <- setClass("myReg", slots = c(title = "character"), contains = "mlm")

fit2 <- myReg(fit, title = "Sepal Regression for iris data")

fit2 # shows the inherited "mlm" object and the title

identical(S3Part(fit2), as(fit2, "mlm"))

class(as(fit2, "mlm")) # the S4 class, "mlm"

class(as(fit2, "S3")) # the S3 class, c("mlm", "lm")

## An object may contain an S3 class from a subclass of that declared:
xlm <- setClass("xlm", slots = c(eps = "numeric"), contains = "lm")

xfit <- xlm(fit, eps = .Machine$double.eps)

xfit@.S3Class # c("mlm", lm")

\dontshow{
    removeClass("myReg")
}
}
\keyword{ programming }
\keyword{ classes }
