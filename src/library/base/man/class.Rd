% File src/library/base/man/class.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{class}
\title{Object Classes}
\alias{class}
\alias{.class2}
\alias{class<-}
\alias{oldClass}
\alias{oldClass<-}
\alias{unclass}
\alias{inherits}
\alias{isa}
\alias{nameOfClass}
\alias{nameOfClass.default}
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
nameOfClass(x)
isa(x, what)

oldClass(x)
oldClass(x) <- value
.class2(x)
}
\arguments{
  \item{x}{an \R object.}
  \item{what, value}{a character vector naming classes. \code{value}
    can also be \code{NULL}. \code{what} can also be a
    non-character R object with a \code{nameOfClass()} method.}
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
  % and when set directly, uses yet different C code classgets()

  %% the (n == 0) clause in R_data_class():
  If the object does not have a class attribute, it has an implicit
  class, notably \code{"matrix"}, \code{"array"}, \code{"function"} or
  \code{"numeric"} or the result of
  \code{\link{typeof}(x)} (which is similar to \code{\link{mode}(x)}),
  but for type \code{"language"} and \code{\link{mode}} \code{"call"},
  where the following extra classes exist for the corresponding function
  \code{\link{call}}s:
  \code{if}, \code{for}, \code{while}, \code{(}, \code{\{}, \code{<-}, \code{=}.

  Note that for objects \code{x} of an implicit (or an S4) class, when a
  (S3) generic function \code{foo(x)} is called, method dispatch may use
  more classes than are returned by \code{class(x)}, e.g., for a numeric
  matrix, the \code{foo.numeric()} method may apply.  The exact full
  \code{\link{character}} vector of the classes which
  \code{\link{UseMethod}()} uses, is available as \code{.class2(x)} since
  \R version 4.0.0.  (This also applies to S4 objects when S3 dispatch is
  considered, see below.)

  Beware that using \code{.class2()} for other reasons than didactical,
  diagnostical or for debugging may rather be a misuse than smart.

  \code{\link{NULL}} objects (of implicit class \code{"NULL"}) cannot have
  attributes (hence no \code{class} attribute) and attempting to assign a
  class is an error.

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
  classes an object inherits from.  Assigning an empty character vector or
  \code{NULL} removes the class attribute, as for \code{oldClass<-} or
  direct attribute setting.  Whereas it is clearer to explicitly assign
  \code{NULL} to remove the class, using an empty vector is more natural in
  e.g., \code{class(x) <- \link{setdiff}(class(x), "ts")}.

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

  \code{nameOfClass} is an S3 generic. It is called by \code{inherits} to 
  get the class name for \code{what}, allowing for \code{what} to be 
  values other than a character vector. \code{nameOfClass} methods are 
  expected to return a character vector of length 1.

  \code{isa} tests whether \code{x} is an object of class(es) as given
  in \code{what} by using \code{\link[methods]{is}} if \code{x} is an S4
  object, and otherwise giving \code{TRUE} iff \emph{all} elements of
  \code{class(x)} are contained in \code{what}.

  All but \code{inherits} and \code{isa} are \link{primitive} functions.
}

\note{
  \code{\link{UseMethod}} dispatches on the class as returned by
  \code{class} (with some interpolated classes: see the link) rather
  than \code{oldClass}.  \emph{However}, \link{group generic}s dispatch
  on the \code{oldClass} for efficiency, and \link{internal generic}s
  only dispatch on objects for which \code{\link{is.object}} is true.
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

.class2(pi)               # "double" "numeric"
.class2(matrix(1:6, 2,3)) # "matrix" "array" "integer" "numeric"
}
\keyword{methods}
\keyword{classes}
