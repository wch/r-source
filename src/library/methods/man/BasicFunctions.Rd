\name{BasicFunctions}
\alias{Ops}
\alias{Arith}
\alias{Math}
\alias{Math2}
\alias{Summary}
\alias{Compare}
\alias{Complex}
\alias{.makeBasicFunsList}

%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Group Generic Functions }
\description{
  These are group generic functions.  Methods defined for them will be
  used for any of the specific functions belonging to the particular
  group, provided no specific methods override.

  These functions should never be called directly (a suitable error
  message will result if they are).
}
\usage{
Arith(e1, e2)
Compare(e1, e2)
Ops(e1, e2)

Math(x)
Math2(x, digits)

Summary(x, ..., na.rm = FALSE)

Complex(z)

}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{e1, e2}{ Arguments to the various binary operators. }

  \item{x}{ The argument to the \code{Math} or  \code{Summary} groups
      of functions.}
}
\details{

  The functions belonging to the various groups are as follows:
  \describe{
    \item{\code{Arith}}{\code{"+", "-", "*", "^", "\%\%", "\%/\%", "/"}}
    \item{\code{Compare}}{\code{"==", ">", "<", "!=", "<=", ">="}}
    \item{\code{Ops}}{\code{"Arith", "Compare"}}

   \item{\code{Math}}{\code{"log", "sqrt", "log10", "cumprod", "abs",
       "acos", "acosh", "asin", "asinh", "atan", "atanh", "ceiling",
       "cos", "cosh", "cumsum", "exp", "floor", "gamma", "lgamma",
       "sin", "sinh", "tan", "tanh", "trunc"}}
   \item{\code{Math2}}{\code{"round", "signif"}}

   \item{\code{Summary}}{\code{"max", "min", "range", "prod", "sum",
       "any", "all"}}

   \item{\code{Complex}}{\code{"Arg", "Conj", "Im", "Mod", "Re"}}
  }

All the functions in these groups (other than the group generics
themselves) are basic functions in R.  They are not by default generic
functions, and many of them are defined as primitives, meaning that
they do not have formal arguments.  However, you can still define
methods for them.  The effect of doing so is to create a generic
function with the appropriate arguments, in the environment where the
method definition is to be stored.  It all works more or less as you
might expect, admittedly via a bit of trickery in the background.  
  
}
\references{
  The web page \url{http://www.omegahat.org/RSMethods/index.html} is the
  primary documentation.

  The functions in this package emulate the facility for classes and
  methods described in \emph{Programming with Data} (John M. Chambers,
  Springer, 1998).  See this book for further details and examples.
}
\author{
  John Chambers
}
\keyword{internal}
