\name{groupGeneric}
\alias{.Method}
\alias{.Generic}
\alias{.Group}
\alias{.Class}
\alias{Math}
\alias{Math.data.frame}
\alias{Math.factor}
\alias{Ops}
\alias{Ops.data.frame}
\alias{Ops.factor}
\alias{Ops.ordered}
\alias{Summary}
\alias{Summary.data.frame}
\alias{Summary.factor}
\alias{Arith}
\alias{Compare}
\alias{Complex}
\alias{Math2}
\alias{group generic} % used in class.Rd
\concept{group generic}
\title{Group Generic Functions}
\description{
  Group generic functions can be defined with either S3 and S4 methods
  (with different groups).  Methods are defined for the group of functions
  as a whole.

  A method defined for an individual member of the group takes
  precedence over a method defined for the group as a whole.

  When package \pkg{methods} is attached there are objects visible with
  the names of the group generics: these functions should never be
  called directly (a suitable error message will result if they are).
}
\synopsis{
Math.data.frame(x, \dots)
Math.factor(x, \dots)

Ops.data.frame(e1, e2 = NULL)
Ops.factor(e1, e2)
Ops.ordered(e1, e2)

Summary.data.frame(x, \dots)
Summary.factor(x, \dots)
}
\usage{
## S3 methods have prototypes:
Math(x, \dots)
Ops(e1, e2)
Summary(x, \dots)
Complex(z)

## S4 methods have prototypes:
Arith(e1, e2)
Compare(e1, e2)
Ops(e1, e2)
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

\section{S3 Group Dispatching}{
  %% --------------- grep -nw DispatchGroup src/*/*[ch]

  There are four \emph{groups} for which S3 methods can be written,
  namely the \code{"Math"}, \code{"Ops"}, \code{"Summary"} and
  \code{"Complex"} groups.  These are not \R objects, but methods can be
  supplied for them and base \R contains \code{\link{factor}},
  \code{\link{data.frame}} and \code{\link{difftime}} methods for
  the first three groups.
  (There are also a \code{\link{ordered}} method for \code{Ops},
  \code{\link{POSIXt}} methods for \code{Math} and \code{Ops}, as well as a
  \code{\link{ts}} method for \code{Ops} in package \pkg{stats}.)

%   A function \emph{\code{f}} belonging to one of these groups must be
%   \code{\link{.Internal}} or \code{\link{.Primitive}} and will
%   automatically be using \emph{\code{<grp>.<class> (ob)}} when
%   \emph{\code{f(<ob>)}} is called, \emph{\code{f}} belongs to group
%   \emph{\code{<grp>}} and \emph{\code{<ob>}} is of
%   \code{\link[base]{class}} \emph{\code{<class>}}.

  \enumerate{
    \item Group \code{"Math"}:
    \itemize{
      \item
      \code{abs}, \code{sign}, \code{sqrt}, \cr
      \code{floor}, \code{ceiling}, \code{trunc},\cr
      \code{round}, \code{signif}

      \item \code{exp}, \code{log}, \cr
      \code{cos}, \code{sin}, \code{tan},\cr
      \code{acos}, \code{asin}, \code{atan}

      \code{cosh}, \code{sinh}, \code{tanh},\cr
      \code{acosh}, \code{asinh}, \code{atanh}

      \item
      \code{lgamma}, \code{gamma}, \code{gammaCody},\cr
      \code{digamma}, \code{trigamma}
      %   do_math1() [arithmetic.c:794]: if (DispatchGroup("Math",...))
      %
      %
      % "atan", "round", "log", "signif":
      % do_atan()  [arithmetic.c:958]: if (DispatchGroup("Math", ..))
      % do_round() [arithmetic.c:981]: if (DispatchGroup("Math", ..))
      % do_log()   [arithmetic.c:1011]:if (DispatchGroup("Math", ..))
      % do_signif()[arithmetic.c:1034]:if (DispatchGroup("Math", ..))

      \item \code{cumsum}, \code{cumprod}, \code{cummax}, \code{cummin}
      % do_cum()   [cum.c:140]:    if (DispatchGroup("Math", ...))
    }

    \item Group \code{"Ops"}:
    \itemize{
      \item
      \code{"+"}, \code{"-"}, \code{"*"}, \code{"/"},
      \code{"^"}, \code{"\%\%"}, \code{"\%/\%"}
      % do_arith() [arithmetic.c:240]: if (DispatchGroup("Ops", ...))

      \item \code{"&"}, \code{"|"}, \code{"!"}
      % do_logic() [logic.c:32]:   if (DispatchGroup("Ops",...))

      \item \code{"=="}, \code{"!="},
      \code{"<"}, \code{"<="}, \code{">="}, \code{">"}
      % do_relop() [relop.c:35]:   if (DispatchGroup("Ops", ...))
    }

    \item Group \code{"Summary"}:
    \itemize{
      \item \code{all}, \code{any}
      % do_logic3()[logic.c:278]:  if(DispatchGroup("Summary", ...))
      \item \code{sum}, \code{prod}
      % /*NOMORE:\code{mean}, */
      \item \code{min}, \code{max}
      % do_summary() [summary.c:322]: if(DispatchGroup("Summary",...))
      \item \code{range}
    }

    \item Group \code{Complex}:
    \itemize{
      \item \code{Arg}, \code{Conj}, \code{Im}, \code{Mod}, \code{Re}
      % do_cmathfuns() [complex.c:267]: if(DispatchGroup("Complex",...))
    }
  }

  Note that a method will used for either one of these groups or one of
  its members \emph{only} if it corresponds to a \code{"class"}
  attribute, as the internal code dispatches on \code{\link{oldClass}}
  and not on \code{\link{class}}.  This is for efficiency: having to
  dispatch on, say, \code{Ops.integer} would be too slow.

  The number of arguments supplied for \code{"Math"} group generic
  methods is not checked prior to dispatch. (Most have default methods
  expecting one argument, but three expect two.)
}
\section{S4 Group Dispatching}{
  When package \pkg{methods} is attached (which it is by default),
  formal (S4) methods can be defined for groups.

  The functions belonging to the various groups are as follows:
  \describe{
    \item{\code{Arith}}{\code{"+"}, \code{"-"}, \code{"*"}, \code{"^"},
      \code{"\%\%"}, \code{"\%/\%"}, \code{"/"}}
    \item{\code{Compare}}{\code{"=="}, \code{">"}, \code{"<"},
      \code{"!="}, \code{"<="}, \code{">="}}
    \item{\code{Ops}}{\code{"Arith"}, \code{"Compare"}}
    \item{\code{Math}}{\code{"log"}, \code{"sqrt"}, \code{"log10"},
      \code{"cumprod"}, \code{"abs"}, \code{"acos"}, \code{"acosh"},
      \code{"asin"}, \code{"asinh"}, \code{"atan"}, \code{"atanh"},
      \code{"ceiling"}, \code{"cos"}, \code{"cosh"}, \code{"cumsum"},
      \code{"exp"}, \code{"floor"}, \code{"gamma"}, \code{"lgamma"},
      \code{"sin"}, \code{"sinh"}, \code{"tan"}, \code{"tanh"},
      \code{"trunc"}}
    \item{\code{Math2}}{\code{"round"}, \code{"signif"}}
    \item{\code{Summary}}{\code{"max"}, \code{"min"}, \code{"range"},
      \code{"prod"}, \code{"sum"}, \code{"any"}, \code{"all"}}
    \item{\code{Complex}}{\code{"Arg"}, \code{"Conj"}, \code{"Im"},
      \code{"Mod"}, \code{"Re"}}
  }
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
}
\references{
  Appendix A, \emph{Classes and Methods} of\cr
  Chambers, J. M.  and Hastie, T. J. eds (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.

  Chambers, J. M. (1998) \emph{Programming with Data.} Springer, pp. 352--4.
}
\seealso{
  \code{\link{methods}} for methods of non-Internal generic functions.
}
\examples{
methods("Math")
methods("Ops")
methods("Summary")

d.fr <- data.frame(x=1:9, y=rnorm(9))
data.class(1 + d.fr) == "data.frame" ##-- add to d.f. ...

if(.isMethodsDispatchOn()) {# package "methods" is attached or loaded
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
}
}
\keyword{methods}
