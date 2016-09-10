% File src/library/methods/man/S4groupGeneric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{S4groupGeneric}
\alias{S4groupGeneric}
\alias{GroupGenericFunctions}
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
  Methods can be defined for \emph{group generic functions}.  Each group
  generic function has a number of \emph{member} generic functions
  associated with it.

  Methods  defined for a group generic function cause the same
  method to be defined for each member of the group, but a method explicitly
  defined for a  member of the group takes precedence over a
  method defined, with the same signature, for the group generic.

  The functions shown in this documentation page all reside in the
  \pkg{methods} package, but the mechanism is available to any
  programmer, by calling \code{\link{setGroupGeneric}} (provided package
  \pkg{methods} is attached).
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
  Methods can be defined for the group generic functions by calls to
  \code{\link{setMethod}} in the usual way.
  Note that the group generic functions
   should never be called directly
  -- a suitable error message will result if they are.  When metadata
  for a group generic is loaded, the methods defined become methods
  for the members of the group, but only if no method has been
  specified directly for the member function for the same signature.
  The effect is that group generic definitions are selected before
  inherited methods but after directly specified methods.  For more on
  method selection, see \code{\link{Methods}}.

There are also
  S3 groups \code{Math}, \code{Ops}, \code{Summary} and
  \code{Complex}, see \code{?\link{S3groupGeneric}},
  with no corresponding \R objects, but these are irrelevant for S4
  group generic functions.

  The members of the group defined by a particular generic can be
  obtained by calling \code{\link{getGroupMembers}}. For the group
  generic functions currently defined in this package the members are
  as follows:
  \describe{
    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"},
      \code{"!="}, \code{"<="}, \code{">="}}
    \item{\code{Logic}}{\code{"&"}, \code{"|"}.
    }
    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}, \code{"Logic"}}
    \item{\code{Math}}{\code{"abs"}, \code{"sign"}, \code{"sqrt"},
      \code{"ceiling"}, \code{"floor"}, \code{"trunc"},
      \code{"cummax"}, \code{"cummin"}, \code{"cumprod"}, \code{"cumsum"},
      \code{"log"}, \code{"log10"}, \code{"log2"}, \code{"log1p"},
      \code{"acos"}, \code{"acosh"},
      \code{"asin"}, \code{"asinh"}, \code{"atan"}, \code{"atanh"},
      \code{"exp"}, \code{"expm1"},
      \code{"cos"}, \code{"cosh"}, \code{"cospi"},
      \code{"sin"}, \code{"sinh"}, \code{"sinpi"},
      \code{"tan"}, \code{"tanh"}, \code{"tanpi"},
      \code{"gamma"}, \code{"lgamma"}, \code{"digamma"},
      \code{"trigamma"}
    }
    \item{\code{Math2}}{\code{"round"}, \code{"signif"}}
    \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
      \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
    \item{\code{Complex}}{\code{"Arg"}, \code{"Conj"}, \code{"Im"},
      \code{"Mod"}, \code{"Re"}}
  }
  Note that \code{Ops} merely consists of three sub groups.

  All the functions in these groups (other than the group generics
  themselves) are basic functions in \R.  They are not by default S4 generic
  functions, and many of them are defined as primitives.  However, you can still define
  formal methods for them, both individually and via the group generics.  It all works more or less as you
  might expect, admittedly via a bit of trickery in the background.
  See \link{Methods} for details.

  Note that two members of the \code{Math} group, \code{\link{log}} and
  \code{\link{trunc}}, have \dots as an extra formal argument.
  Since methods for \code{Math} will have only one formal argument,
  you must set a specific method for these functions in order to call
  them with the extra argument(s).

  For further details about group generic functions see section 10.5 of
  \emph{Software for Data Analysis}.

}
\references{
 Chambers, John M. (2008)
 \emph{Software for Data Analysis: Programming with R}
  Springer.  (For the R version.)

 Chambers, John M. (1998)
 \emph{Programming with Data}
 Springer (For the original S4 version).
}
\seealso{ The function \code{\link{callGeneric}} is nearly always
  relevant when writing a method for a group generic.  See the
  examples below and in section 10.5 of \emph{Software for Data Analysis}.

  See \link{S3groupGeneric} for S3 group generics.
}
\examples{
setClass("testComplex", slots = c(zz = "complex"))
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
