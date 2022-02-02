% File src/library/methods/man/setClassUnion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

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
  \item{members}{ the names of the classes that should be members of this union.}
  \item{where}{ where to save the new class definition.  In calls from
    a package's source code, should be omitted to save the definition
    in the package's namespace.}


  \item{Class}{ the name or definition of a class.
    }
}
\details{
  The classes in \code{members} must be defined before creating the
      union.  However, members can be added later on to an existing
      union, as shown in the example below. Class unions can be
      members of other class unions.

 
  Class unions are the only way to create a new superclass of
      a class whose definition is sealed.  The namespace of all
      packages is sealed when the package is loaded, protecting the
      class and other definitions from being overwritten from another
      class or from the global environment.  A call to
      \code{\link{setIs}} that tried to define a new superclass for
      class \code{"numeric"}, for example, would cause an error.

   Class unions are the exception; the class union
   \code{"maybeNumber"} in the examples defines itself as a new
   superclass of \code{"numeric"}.  Technically, it does not alter the
   metadata object in the other package's namespace and, of course,
   the effect of the class union depends on loading the package it
   belongs to.  But, basically, class unions are sufficiently useful
   to justify the exemption.

  The different behavior for class unions is made possible because the
  class definition object for class unions has itself a special class,
  \code{"ClassUnionRepresentation"}, an extension of class
  \code{\linkS4class{classRepresentation}}.
  }

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}


\examples{
## a class for either numeric or logical data
setClassUnion("maybeNumber", c("numeric", "logical"))

## use the union as the data part of another class
setClass("withId", contains = "maybeNumber", slots = c(id = "character"))

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
removeClass("withId")
removeClass("maybeNumber")
}

}
\keyword{programming}
\keyword{classes}
