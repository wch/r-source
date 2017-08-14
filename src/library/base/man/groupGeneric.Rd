% File src/library/base/man/groupGeneric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{groupGeneric}
\alias{S3groupGeneric}
\alias{groupGeneric}
\alias{.Group}
\alias{Math}
\alias{Math.data.frame}
\alias{Ops}
\alias{Ops.data.frame}
\alias{Summary}
\alias{Summary.data.frame}
\alias{Complex}
\alias{group generic} % used in class.Rd and zMethods.Rd
\concept{group generic}
\title{S3 Group Generic Functions}
\description{
  Group generic methods can be defined for four pre-specified groups of
  functions, \code{Math}, \code{Ops}, \code{Summary} and \code{Complex}.
  (There are no objects of these names in base \R, but there are in the
  \pkg{methods} package.)

  A method defined for an individual member of the group takes
  precedence over a method defined for the group as a whole.
}
\usage{
## S3 methods for group generics have prototypes:
\special{Math(x, \dots)}
\special{Ops(e1, e2)}
\special{Complex(z)}
\special{Summary(\dots, na.rm = FALSE)}
}
\arguments{
  \item{x, z, e1, e2}{objects.}
  \item{\dots}{further arguments passed to methods.}
  \item{na.rm}{logical: should missing values be removed?}
}

\details{
  %% --------------- grep -nw DispatchGroup src/*/*[ch]

  There are four \emph{groups} for which S3 methods can be written,
  namely the \code{"Math"}, \code{"Ops"}, \code{"Summary"} and
  \code{"Complex"} groups.  These are not \R objects in base \R, but
  methods can be supplied for them and base \R contains
  \code{\link{factor}}, \code{\link{data.frame}} and
  \code{\link{difftime}} methods for the first three groups.  (There is
  also a \code{\link{ordered}} method for \code{Ops},
  \code{\link{POSIXt}} and \code{\link{Date}} methods for \code{Math}
  and \code{Ops}, \code{\link{package_version}} methods for \code{Ops}
  and \code{Summary}, as well as a \code{\link{ts}} method for
  \code{Ops} in package \pkg{stats}.)

  \enumerate{
    \item Group \code{"Math"}:
    \itemize{
      \item
      \code{abs}, \code{sign}, \code{sqrt},\cr
      \code{floor}, \code{ceiling}, \code{trunc},\cr
      \code{round}, \code{signif}

      \item
      \code{exp}, \code{log},  \code{expm1}, \code{log1p},\cr
      \code{cos}, \code{sin}, \code{tan},\cr
      \code{cospi}, \code{sinpi}, \code{tanpi},\cr
      \code{acos}, \code{asin}, \code{atan}

      \code{cosh}, \code{sinh}, \code{tanh},\cr
      \code{acosh}, \code{asinh}, \code{atanh}

      \item
      \code{lgamma}, \code{gamma}, \code{digamma}, \code{trigamma}
      %   do_math1() [../../../main/arithmetic.c:794]: if (DispatchGroup("Math",...))
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
    Members of this group dispatch on \code{x}.  Most members accept
    only one argument, but members \code{log}, \code{round} and
    \code{signif} accept one or two arguments, and \code{trunc} accepts
    one or more.

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
    This group contains both binary and unary operators (\code{+},
    \code{-} and \code{!}): when a unary operator is encountered the
    \code{Ops} method is called with one argument and \code{e2} is
    missing.

    The classes of both arguments are considered in dispatching any
    member of this group.  For each argument its vector of classes is
    examined to see if there is a matching specific (preferred) or
    \code{Ops} method.  If a method is found for just one argument or
    the same method is found for both, it is used.
    If different methods are found, there is a warning about
    \sQuote{incompatible methods}: in that case or if no method is found
    for either argument the internal method is used.

    Note that the \code{\link{data.frame}} methods for the comparison
    (\code{"Compare"}: \code{==}, \code{<}, \dots) and logic
    (\code{"Logic"}: \code{&} \code{|} and \code{!}) operators return a
    logical \code{\link{matrix}} instead of a data frame, for
    convenience and back compatibility.

    If the members of this group are called as functions, any argument
    names are removed to ensure that positional matching is always used.

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
    Members of this group dispatch on the first argument supplied.

    \item Group \code{"Complex"}:
    \itemize{
      \item \code{Arg}, \code{Conj}, \code{Im}, \code{Mod}, \code{Re}
      % do_cmathfuns() [complex.c:267]: if(DispatchGroup("Complex",...))
    }
    Members of this group dispatch on \code{z}.
  }

  Note that a method will be used for one of these groups or one of its
  members \emph{only} if it corresponds to a \code{"class"} attribute,
  as the internal code dispatches on \code{\link{oldClass}} and not on
  \code{\link{class}}.  This is for efficiency: having to dispatch on,
  say, \code{Ops.integer} would be too slow.

  The number of arguments supplied for primitive members of the
  \code{"Math"} group generic methods is not checked prior to dispatch.

  There is no lazy evaluation of arguments for group-generic functions.
}
\section{Technical Details}{
  These functions are all primitive and \link{internal generic}.

  The details of method dispatch and variables such as \code{.Generic}
  are discussed in the help for \code{\link{UseMethod}}.  There are a
  few small differences:
  \itemize{
    \item For the operators of group \code{Ops}, the object
    \code{.Method} is a length-two character vector with elements the
    methods selected for the left and right arguments respectively.  (If
    no method was selected, the corresponding element is \code{""}.)
    \item Object \code{.Group} records the group used for dispatch (if
    a specific method is used this is \code{""}).
  }
}
\note{
  Package \pkg{methods} does contain objects with these names, which it
  has re-used in confusing similar (but different) ways.  See the help
  for that package.
}
\references{
  Appendix A, \emph{Classes and Methods} of\cr
  Chambers, J. M.  and Hastie, T. J. eds (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{methods}} for methods of non-internal generic functions.

  \link{S4groupGeneric} for group generics for S4 methods.
}
\examples{
require(utils)

d.fr <- data.frame(x = 1:9, y = stats::rnorm(9))
class(1 + d.fr) == "data.frame" ##-- add to d.f. ...

methods("Math")
methods("Ops")
methods("Summary")
methods("Complex")  # none in base R
}
\keyword{methods}
