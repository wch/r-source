% File src/library/utils/man/methods.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{methods}
\title{List Methods for S3 Generic Functions or Classes}
\alias{.S3methods}
\alias{methods}
\alias{format.MethodsFunction}
\alias{print.MethodsFunction}
\description{
  List all available methods for a S3 and S4 generic function, or all
  methods for an S3 or S4 class.
}
\usage{% --> ../R/objects.R :
   methods(generic.function, class, all.names = FALSE, dropPath = FALSE)
.S3methods(generic.function, class, envir = parent.frame(),
                                    all.names = FALSE, dropPath = FALSE, useEnv = FALSE)

\S3method{format}{MethodsFunction}(x, byclass = attr(x, "byclass"), \dots)
\S3method{print}{MethodsFunction}(x, byclass = attr(x, "byclass"), \dots)
}
\arguments{
  \item{generic.function}{a generic function, or a character string naming a
    generic function.}
  \item{class}{a symbol or character string naming a class: only used if
    \code{generic.function} is not supplied.}
  \item{envir}{the environment in which to look for the definition of
    the generic function, when the generic function is passed as a
    character string.}
  \item{all.names}{a \code{\link{logical}} indicating if all
    object names are returned.  When \code{FALSE} as by default, names
    beginning with a \samp{.} are omitted.}
  \item{dropPath}{a \code{\link{logical}} indicating if the
    \code{\link{search}()} path, apart from \code{\link{.GlobalEnv}} and
    \code{package:base} (i.e., \code{\link{baseenv}()}), should be skipped
    when searching for method definitions.  The default \code{FALSE} is
    back compatible and typically desired for \I{\code{print()}ing}, with or
    without asterisk; \code{dropPath=TRUE} has been hard coded in \R 4.3.0
    and is faster for non-small \code{\link{search}()} paths.}
  \item{useEnv}{a \code{\link{logical}} indicating if \code{envir} should
    be searched in addition to the (much reduced in case of \code{dropPath=TRUE})
    \code{\link{search}()} path.}
  \item{x}{typically the result of \code{methods(..)}, an \R object of S3
    class \code{"MethodsFunction"}, see \sQuote{Value} below.}
  \item{byclass}{an optional \code{\link{logical}} allowing to override
    the \code{"byclass"} attribute determining how the result is
    printed, see \sQuote{Details}.}
  \item{\dots}{potentially further arguments passed to and from methods;
    unused currently.}
}

\details{
  \code{methods()} finds S3 and S4 methods associated with either the
  \code{generic.function} or \code{class} argument.
  Methods found are those provided by all loaded namespaces via
  registration, see \code{\link{UseMethod}}; normally, this includes
  all packages on the current \code{search()} path.  \code{.S3methods()}
  finds only S3 methods, \code{\link{.S4methods}()} finds only S4 methods.

  When invoked with the \code{generic.function} argument, the
  \code{"byclass"} attribute (see Details) is \code{FALSE}, and the
  \code{print} method by default displays the signatures (full names) of
  S3 and S4 methods.  S3 methods are printed by pasting the generic
  function and class together, separated by a \sQuote{.}, as
  \code{generic.class}.  The S3 method name is followed by an asterisk
  \code{*} if the method definition is not exported from the package
  namespace in which the method is defined.  S4 method signatures are
  printed as \code{generic,class-method}; S4 allows for multiple
  dispatch, so there may be several classes in the signature
  \code{generic,A,B-method}.

  When invoked with the \code{class} argument, \code{"byclass"} is
  \code{TRUE}, and the \code{print} method by default displays the names
  of the generic functions associated with the class, \code{generic}.

  The source code for all functions is available.  For S3 functions
  exported from the namespace, enter the method at the command line as
  \code{generic.class}.  For S3 functions not exported from the
  namespace, see \code{getAnywhere} or \code{getS3method}.  For S4
  methods, see \code{getMethod}.

  Help is available for each method, in addition to each generic.  For
  interactive help, use the documentation shortcut \code{?} with the
  name of the generic and tab completion, \code{?"generic<tab>"} to
  select the method for which help is desired.

  The S3 functions listed are those which \emph{are named like methods}
  and may not actually be methods (known exceptions are discarded in the
  code).
}

\value{
  An object of class \code{"MethodsFunction"}, a character vector of
  method names with \code{"byclass"} and \code{"info"} attributes.  The
  \code{"byclass"} attribute is a \code{\link{logical}} indicating if
  the results were obtained with argument \code{class}
  defined.  The \code{"info"} attribute is a data frame with columns:
  \describe{
    \item{generic}{\code{\link{character}} vector of the names of the generic.}
    \item{visible}{logical(), is the method \dQuote{visible} to the user?
      When true, it typically is exported from the namespace of the package
      in which it is defined, and the package is \code{\link{attach}()}ed
      to the \code{\link{search}()} path.}
    \item{isS4}{logical(), true when the method is an S4 method.}
    \item{from}{a \code{\link{factor}}, the location or package name
      where the method was found.}
  }
}

\note{
  The original \code{methods} function was written by \I{Martin Maechler}.
}

\seealso{
  \code{\link{S3Methods}}, \code{\link{class}}, \code{\link{getS3method}}.

  For S4, \code{\link{getMethod}}, \code{\link{showMethods}},
  \link[methods]{Introduction} or \code{\link{Methods_Details}}.
}

\references{
  Chambers, J. M. (1992)
  \emph{Classes and methods: object-oriented programming in S.}
  Appendix A of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
methods(class = "MethodsFunction") # format and print

require(stats)

methods(summary)
methods(class = "aov")    # S3 class
## The same, with more details and more difficult to read:
print(methods(class = "aov"), byclass=FALSE)
methods("[[")             # uses C-internal dispatching
methods("$")
methods("$<-")            # replacement function
methods("+")              # binary operator
methods("Math")           # group generic
require(graphics)
methods(axis)             # looks like a generic, but is not

mf <- methods(format)     # quite a few; ... the last few :
tail(cbind(meth = format(mf)))

if(require(Matrix, quietly = TRUE)) {
print(methods(class = "Matrix"))  # S4 class
m <- methods(dim)         # S3 and S4 methods
print(m)
print(attr(m, "info"))    # more extensive information

## --> help(showMethods) for related examples
}
}
\keyword{methods}
