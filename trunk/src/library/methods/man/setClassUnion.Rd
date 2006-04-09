\name{setClassUnion}
\alias{setClassUnion}
\alias{isClassUnion}
\alias{ClassUnionRepresentation-class}
\title{Classes Defined as the Union of Other Classes}
\description{
  A class may be defined as the \emph{union} of other classes; that
  is, as a virtual class defined as a superclass of several other
  classes. Class unions are useful in method signatures or as slots in
  other classes, when we want to allow one of several classes to be supplied.
}
\usage{
setClassUnion(name, members, where)
isClassUnion(Class)
}
\arguments{
  \item{name}{ the name for the new union class. }
  \item{members}{ the classes that should be members of this union.}
  \item{where}{ where to save the new class definition; by default,
      the environment of the package in which the \code{setClassUnion}
      call appears, or the global environment if called outside of the
      source of a package.}


  \item{Class}{ the name or definition of a class.
    }
}
\details{
  The classes in \code{members} must be defined before creating the
      union.  However, members can be added later on to an existing
      union, as shown in the example below. Class unions can be
      members of other class unions.

  Class unions are the only way to create a class that is extended by
      a class whose definition is sealed (for example, the
      basic datatypes or other classes defined in the base or methods
      package in R are sealed).  You cannot say \code{setIs("function", "other")}
      unless \code{"other"} is a class union.  In general, a
      \code{setIs} call of this form changes the definition of the
      first class mentioned (adding \code{"other"} to the list of
      superclasses contained in the definition of \code{"function"}).

      Class unions get around this by not modifying the first class
  definition, relying instead on storing information in the subclasses
  slot of the class union.  In order for this technique to work, the
  internal computations for expressions such as
  \code{\link{extends}(class1, class2)} work
  differently for class unions than for regular classes; specifically,
  they test whether any class is in common between the superclasses of
  \code{class1} and the subclasses of \code{class2}.

  The different behavior for class unions is made possible because the
  class definition object for class unions has itself a special class,
  \code{"ClassUnionRepresentation"}, an extension of
  \code{"classRepresentation"} (see \link{classRepresentation-class}).
  }

\references{
  The R package \pkg{methods} implements, with a few exceptions, the
  programming interface for classes and methods in the book
  \emph{Programming with Data} (John M. Chambers, Springer, 1998), in
  particular sections 1.6, 2.7, 2.8, and chapters 7 and 8.

  While the programming interface for the \pkg{methods} package follows
  the reference, the R software is an original implementation, so
  details in the reference that reflect the S4 implementation may appear
  differently in R.  Also, there are extensions to the programming
  interface developed more recently than the reference.  For a
  discussion of details and ongoing development, see the web page
  \url{http://developer.r-project.org/methodsPackage.html} and the
  pointers from that page.
}

\examples{
## a class for either numeric or logical data
setClassUnion("maybeNumber", c("numeric", "logical"))

## use the union as the data part of another class
setClass("withId", representation("maybeNumber", id = "character"))

w1 <- new("withId", 1:10, id = "test 1")
w2 <- new("withId", sqrt(w1)\%\%1 < .01, id = "Perfect squares")

## add class "complex" to the union "maybeNumber"
setIs("complex", "maybeNumber")

w3 <- new("withId", complex(real = 1:10, imaginary = sqrt(1:10)))

## a class union containing the existing class  union "OptionalFunction"
setClassUnion("maybeCode",
    c("expression", "language", "OptionalFunction"))

is(quote(sqrt(1:10)), "maybeCode")  ## TRUE
\dontshow{
## The following test is less trivial than it looks.
## It depends on the assignment of the data part NOT performing a
## strict coerce to "numeric" on the way to satisfying
## is(ttt, "maybeNumber").
stopifnot(identical(w1@.Data, 1:10))
}

}
\keyword{programming}
\keyword{classes}
