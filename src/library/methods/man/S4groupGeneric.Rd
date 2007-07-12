\name{S4groupGeneric}
\alias{S4groupGeneric}
\alias{Math}
\alias{Ops}
\alias{Summary}
\alias{Arith}
\alias{Logic}
\alias{Compare}
\alias{Complex}
\alias{Math2}
\title{S4 Group Generic Functions}
\description{
  Methods can be defined for groups of functions known as \emph{group
    generic functions}.  These exist in both S3 (see
  \link{S3groupGeneric}) and S4 flavours, with different groups.

  Methods are defined for the group of functions as a whole.  A method
  defined for an individual member of the group takes precedence over a
  method defined for the group as a whole.

  When package \pkg{methods} is attached there are objects visible with
  the names of the group generics: these functions should never be
  called directly (a suitable error message will result if they are).
}
\usage{
## S4 group generics:
Arith(e1, e2)
Compare(e1, e2)
Ops(e1, e2)
Logic(e1, e2)
Math(x)
Math2(x, digits)
Summary(x, \dots, na.rm = FALSE)
Complex(z)
}
\arguments{
  \item{x, z, e1, e2}{objects.}
  \item{digits}{number of digits to be used in \code{round} or \code{signif}.}
  \item{\dots}{further arguments passed to or from methods.}
  \item{na.rm}{logical: should missing values be removed?}
}

\details{
  When package \pkg{methods} is attached (which it is by default),
  formal (S4) methods can be defined for the group generic functions
  (which are \R objects which should never be called directly
  -- a suitable error message will result if they are).  There are also
  S3 groups \code{Math}, \code{Ops}, \code{Summary} and
  \code{Complex}, see \code{?\link{S3groupGeneric}},
  with no corresponding \R objects.

  The functions belonging to the various groups are as follows:
  \describe{
    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"},
      \code{"!="}, \code{"<="}, \code{">="}}
    \item{\code{Logic}}{\code{"&"}, \code{"|"}, but \bold{not}
      \code{"!"} since that has only one argument.  Note that this is
      contrary to Chambers(1998), on purpose.
    }
    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
    \item{\code{Math}}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
      \code{"ceiling"}, \code{"floor"}, \code{"trunc"},
      \code{"cummax"}, \code{"cummin"}, \code{"cumprod"}, \code{"cumsum"},
      \code{"log"}, \code{"log10"}, \code{"log2"}, \code{"log1p"},
      \code{"acos"}, \code{"acosh"},
      \code{"asin"}, \code{"asinh"}, \code{"atan"}, \code{"atanh"},
      \code{"exp"}, \code{"expm1"}, \code{"cos"}, \code{"cosh"},
      \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"},
      \code{"gamma"}, \code{"lgamma"}, \code{"digamma"},
      \code{"trigamma"}
    }
    \item{\code{Math2}}{\code{"round"}, \code{"signif"}}
    \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
      \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
    \item{\code{Complex}}{\code{"Arg"}, \code{"Conj"}, \code{"Im"},
      \code{"Mod"}, \code{"Re"}}
  }
  Note that \code{"Ops"} merely consists of three sub groups.
  Functions with the group names exist in the \pkg{methods} package but
  should not be called directly.

  All the functions in these groups (other than the group generics
  themselves) are basic functions in \R.  They are not by default S4 generic
  functions, and many of them are defined as primitives, meaning that
  they do not have formal arguments.  However, you can still define
  formal methods for them.  The effect of doing so is to create an S4 generic
  function with the appropriate arguments, in the environment where the
  method definition is to be stored.  It all works more or less as you
  might expect, admittedly via a bit of trickery in the background.

  Note: currently those members which are not primitive functions must
  have been converted to S4 generic functions (preferably
  \emph{before} setting an S4 group generic method) as it only sets
  methods for known S4 generics.  This can be done by a call to
  \code{setGeneric}, for example \code{setGeneric("round", group="Math2")}.
  These are currently all the members of the \code{Math2} and
  \code{Summary} groups.
}
\references{
  Appendix A, \emph{Classes and Methods} of\cr
  Chambers, J. M.  and Hastie, T. J. eds (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998) \emph{Programming with Data.} Springer, pp. 352--4.
}
\seealso{
  \link{S3groupGeneric} for S3 group generics.
}
\examples{
setClass("testComplex", representation(zz = "complex"))
## method for whole group "Complex"
setMethod("Complex", "testComplex",
	  function(z) c("groupMethod", callGeneric(z@zz)))
## exception for Arg() :
setMethod("Arg", "testComplex",
	  function(z) c("ArgMethod", Arg(z@zz)))
z1 <- 1+2i
z2 <- new("testComplex", zz = z1)
stopifnot(identical(Mod(z2), c("groupMethod", Mod(z1))))
stopifnot(identical(Arg(z2), c("ArgMethod", Arg(z1))))
\dontshow{
removeMethods("Complex")
removeMethods("Arg")
}}
\keyword{methods}
