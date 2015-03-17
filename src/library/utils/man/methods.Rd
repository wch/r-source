% File src/library/utils/man/methods.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{methods}
\title{List Methods for S3 Generic Functions or Classes}
\alias{.S3methods}
\alias{methods}
\alias{print.MethodsFunction}
\description{
  List all available methods for a S3 and S4 generic function, or all
  methods for an S3 or S4 class.
}
\usage{
methods(generic.function, class)
.S3methods(generic.function, class)
}
\arguments{
  \item{generic.function}{a generic function, or a character string naming a
    generic function.}
  \item{class}{a symbol or character string naming a class: only used if
    \code{generic.function} is not supplied.}
}

\details{

  \code{methods()} finds S3 and S4 methods associated with either the
  \code{generic.function} or \code{class} argument.  Methods are found in
  all packages on the current \code{search()} path.  \code{.S3methods()}
  finds only S3 methods.

  When invoked with the \code{generic.function} argument, the
  \code{print} method displays the signatures (full names) of S3 and S4
  methods.  S3 methods are printed by pasting the generic function and
  class together, separated by a \sQuote{.}, as
  \code{generic.class}.  The S3 method name is followed by an asterisk
  \code{*} if the method definition is not exported from the package
  namespace in which the method is defined.  S4 method signatures are
  printed as \code{generic,class-method}; S4 allows for multiple
  dispatch, so there may be several classes in the signature
  \code{generic,A,B-method}.

  When invoked with the \code{class} argument, the \code{print} method
  displays the names of the generic functions associated with the class,
  \code{generic}.

  The source code for all functions is available.  For S3 functions
  exported from the namespace, enter the method at the command line as
  \code{generic.class}.  For S3 functions not exported from the
  namespace, see \code{getAnywhere} or \code{getS3method}.  For S4
  methods, see \code{getMethod}.

  Help is available for each method, in addition to each generic.  For
  interactive help, use the documentation shortcut \code{?} with the
  name of the generic and tab completion, \code{?"generic<tab>"} to
  select the method for which help is desired.

  The S3 functions listed are those which are \emph{are named like
  methods} and may not actually be methods (known exceptions are
  discarded in the code).

}

\value{

  An object of class \code{"MethodsFunction"}, a character vector of
  method names with \code{"byclass"} and \code{"info"} attributes.  The
  \code{"byclass"} attribute is a logical(1) vector with value
  \code{TRUE} when the results were obtained with argument \code{class}
  defined.  The \code{"info"} attribute is a data frame with
  columns:
  \describe{
    \item{generic}{character(), the name of the generic.}
    \item{visible}{logical(), is the column exported from the namespace
      of the package in which it is defined?}
    \item{isS4}{logical(), true when the method is an S4 method.}
    \item{from}{factor(), the location or package name where the method
      was found.}
  }
}

\note{
  The original \code{methods} function was written by Martin Maechler.
}

\seealso{
  \code{\link{S3Methods}}, \code{\link{class}}, \code{\link{getS3method}}.

  For S4, \code{\link{getMethod}}, \code{\link{showMethods}},
  \code{\link{Methods}}.
}

\references{
  Chambers, J. M. (1992)
  \emph{Classes and methods: object-oriented programming in S.}
  Appendix A of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}
\examples{
require(stats)

methods(summary)
methods(class = "aov")    # S3 class
methods("[[")             # uses C-internal dispatching
methods("$")
methods("$<-")            # replacement function
methods("+")              # binary operator
methods("Math")           # group generic
require(graphics)
methods("axis")           # looks like a generic, but is not

require(Matrix)
methods(class="Matrix")  # S4 class
methods("dim")           # S3 and S4 methods

m <- methods("dim")
attr(m, "info")          # more extensive information

## --> help(showMethods) for related examples
}
\keyword{methods}
