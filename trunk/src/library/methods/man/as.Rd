% File src/library/methods/man/as.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{as}
\alias{as}
\alias{as<-}
\title{Force an Object to Belong to a Class}
\description{
  Coerce an object to a given class.
}
\usage{
as(object, Class, strict=TRUE, ext)

as(object, Class) <- value

}
\section{Description}{
    \code{as(object)}
      returns the version of this object coerced to be the given
      \code{Class}.  When used in the replacement form on the left of
      an assignment, the portion of the object corresponding to
      \code{Class} is replaced by \code{value}.

      The operation of \code{as()} in either form depends on the
      definition of coerce methods.  Methods are defined automatically
      when the two classes are related by inheritance; that is, when
      one of the classes is a subclass of the other.

      Coerce methods are also predefined for basic classes (including all
      the types of vectors, functions and a few others).

      Beyond these two sources of methods, further methods are defined
      by calls to the \code{\link{setAs}} function.  See that
      documentation also for details of how coerce methods work. Use
      \code{showMethods(coerce)} for a list of all currently defined methods, as in the
      example below.

 
  }
\arguments{
  \item{object}{any \R object.}
  \item{Class}{the name of the class to which \code{object} should be
    coerced. }
  \item{strict}{logical flag.  If \code{TRUE}, the returned object
    must be strictly from the target class (unless that class is a
    virtual class, in which case the object will be from the closest
    actual class, in particular the original object, if that class extends the
    virtual class directly).

    If \code{strict = FALSE}, any simple extension of the target class
    will be returned, without further change.  A simple extension is,
    roughly, one that just adds slots to an existing class.}
  \item{value}{The value to use to modify \code{object} (see the
    discussion below).  You should supply an object with class
    \code{Class}; some coercion is done, but you're unwise to rely on
    it.}
  \item{ext}{an optional object
    defining how \code{Class} is extended by the class of the
    object (as returned by \code{\link{possibleExtends}}).
    This argument is used internally;
    do not use it directly.
  }
}




\section{Basic Coercion Methods}{
  Methods are pre-defined for coercing any object to one of the basic
  datatypes.  For example, \code{as(x, "numeric")} uses the existing
  \code{as.numeric} function.  These and all other existing methods
  can be listed as shown in the example.
}

\seealso{
  If you think of using \code{try(as(x, cl))}, consider
  \code{\link{canCoerce}(x, cl)} instead.
}

\examples{

## Show all the existing methods for as()
showMethods("coerce")

}

\references{
 Chambers, John M. (2016)
 \emph{Extending R},
  Chapman & Hall.
(Chapters 9 and 10.)
}


\keyword{programming}
\keyword{classes}
\keyword{methods}
