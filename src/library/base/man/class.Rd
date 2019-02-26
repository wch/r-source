% File src/library/base/man/class.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{class}
\title{Object Classes}
\alias{class}
\alias{class<-}
\alias{oldClass}
\alias{oldClass<-}
\alias{unclass}
\alias{inherits}
\description{
  \R possesses a simple generic function mechanism which can be used for
  an object-oriented style of programming.  Method dispatch takes place
  based on the class of the first argument to the generic function.
}
\usage{
class(x)
class(x) <- value
unclass(x)
inherits(x, what, which = FALSE)

oldClass(x)
oldClass(x) <- value
}
\arguments{
  \item{x}{a \R object}
  \item{what, value}{a character vector naming classes.  \code{value}
    can also be \code{NULL}.}
  \item{which}{logical affecting return value: see \sQuote{Details}.}
}

\details{
  Here, we describe the so called \dQuote{S3} classes (and methods). For
  \dQuote{S4} classes (and methods), see \sQuote{Formal classes} below.

  %% Implementation: -> R_data_class(*, FALSE) ../../../main/attrib.c
  Many \R objects have a \code{class} attribute, a character vector
  giving the names of the classes from which the object \emph{inherits}.
  (Functions \code{oldClass} and \code{oldClass<-} get and set the
  attribute, which can also be done directly.)

  %% the (n == 0) clause in R_data_class():
  If the object does not have a class attribute, it has an implicit
  class, notably \code{"matrix"}, \code{"array"}, \code{"function"} or
  \code{"numeric"} or the result of
  \code{\link{typeof}(x)} (which is similar to \code{\link{mode}(x)}),
  but for type \code{"language"} and \code{\link{mode}} \code{"call"},
  where the following extra classes exist for the corresponding function
  \code{\link{call}}s:
  \code{if}, \code{while}, \code{for}, \code{=}, \code{<-}, \code{(},
  \code{\{}, \code{call}.

  Note that \code{\link{NULL}} objects cannot have attributes (hence not
  classes) and attempting to assign a class is an error.

  When a generic function \code{fun} is applied to an object with class
  attribute \code{c("first", "second")}, the system searches for a
  function called \code{fun.first} and, if it finds it, applies it to
  the object.  If no such function is found, a function called
  \code{fun.second} is tried.  If no class name produces a suitable
  function, the function \code{fun.default} is used (if it exists).  If
  there is no class attribute, the implicit class is tried, then the
  default method.

  The function \code{class} prints the vector of names of classes an
  object inherits from.  Correspondingly, \code{class<-} sets the
  classes an object inherits from.  Assigning \code{NULL} removes the
  class attribute.

  \code{unclass} returns (a copy of) its argument with its class
  attribute removed.  (It is not allowed for objects which cannot be
  copied, namely environments and external pointers.)

  \code{inherits} indicates whether its first argument inherits from any
  of the classes specified in the \code{what} argument.  If \code{which}
  is \code{TRUE} then an integer vector of the same length as
  \code{what} is returned.  Each element indicates the position in the
  \code{class(x)} matched by the element of \code{what}; zero indicates
  no match. If \code{which} is \code{FALSE} then \code{TRUE} is
  returned by \code{inherits} if any of the names in \code{what} match
  with any \code{class}.

  All but \code{inherits} are \link{primitive} functions.
}

\note{
  Functions \code{oldClass} and \code{oldClass<-} behave in the same way
  as functions of those names in S-PLUS 5/6, \emph{but} in \R
  \code{\link{UseMethod}} dispatches on the class as returned by
  \code{class} (with some interpolated classes: see the link) rather
  than \code{oldClass}.  \emph{However}, \link{group generic}s dispatch
  on the \code{oldClass} for efficiency, and \link{internal generic}s
  only dispatch on objects for which \code{\link{is.object}} is true.

  In older versions of \R, assigning a zero-length vector with
  \code{class} removed the class: it is now an error (whereas it
  still works for \code{oldClass}).  It is clearer to always assign \code{NULL}
  to remove the class.
}

\section{Formal classes}{
  An additional mechanism of \emph{formal} classes, nicknamed
  \dQuote{S4}, is available in package \pkg{methods} which is attached
  by default.  For objects which have a formal class, its name is
  returned by \code{class} as a character vector of length one and
  method dispatch can happen on \emph{several} arguments, instead of
  only the first.  However, S3 method selection attempts to treat objects
  from an S4 class as if they had the appropriate S3 class attribute, as
  does \code{inherits}.  Therefore, S3 methods can be defined for S4
  classes.  See the \sQuote{\link[methods]{Introduction}} and \sQuote{\link{Methods_for_S3}}
  help pages for basic information on S4 methods and for the relation
  between these and S3 methods.

  The replacement version of the function sets the class to the value
  provided.  For classes that have a formal definition, directly
  replacing the class this way is strongly deprecated.  The expression
  \code{\link{as}(object, value)} is the way to coerce an object to a
  particular class.

  The analogue of \code{inherits} for formal classes is
  \code{\link{is}}.  The two functions behave consistently
  with one exception:  S4 classes can have conditional
  inheritance, with an explicit test.  In this case, \code{is} will
  test the condition, but \code{inherits} ignores all conditional
  superclasses.
}

\seealso{
  \code{\link{UseMethod}}, \code{\link{NextMethod}},
  \sQuote{\link{group generic}}, \sQuote{\link{internal generic}}
}
\examples{
x <- 10
class(x) # "numeric"
oldClass(x) # NULL
inherits(x, "a") #FALSE
class(x) <- c("a", "b")
inherits(x,"a") #TRUE
inherits(x, "a", TRUE) # 1
inherits(x, c("a", "b", "c"), TRUE) # 1 2 0

class( quote(pi) )           # "name"
## regular calls
class( quote(sin(pi*x)) )    # "call"
## special calls
class( quote(x <- 1) )       # "<-"
class( quote((1 < 2)) )      # "("
class( quote( if(8<3) pi ) ) # "if"
}
\keyword{methods}
\keyword{classes}
