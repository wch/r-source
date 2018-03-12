% File src/library/base/man/deparseOpts.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{deparseOpts}
\title{Options for Expression Deparsing}
\alias{.deparseOpts}
\alias{..deparseOpts}
\description{
  Process the deparsing options for \code{deparse}, \code{dput} and
  \code{dump}.
}
\usage{
.deparseOpts(control)

..deparseOpts
}
\arguments{
  \item{control}{character vector of deparsing options.}
}
\details{
  \code{..deparseOpts} is the \code{\link{character}} vector of possible
  deparsing options used by \code{.deparseOpts()}.

  \code{.deparseOpts()} is called by \code{\link{deparse}}, \code{\link{dput}} and
  \code{\link{dump}} to process their \code{control} argument.

  The \code{control} argument is a vector containing zero or more of the
  following strings (exactly those in \code{..deparseOpts}).  Partial
  string matching is used.
  \describe{
    \item{\code{"keepInteger"}:}{
      Either surround integer vectors by \code{as.integer()} or use
      suffix \code{L}, so they are not converted to type double when
      parsed.  This includes making sure that integer \code{NA}s are
      preserved (via \code{NA_integer_} if there are no non-\code{NA}
      values in the vector, unless \code{"S_compatible"} is set).
    }
    \item{\code{"quoteExpressions"}:}{
      Surround unevaluated expressions, but not \code{\link{formula}}s,
      with \code{quote()}, so they are not evaluated when re-parsed.
    }
    \item{\code{"showAttributes"}:}{
      If the object has \code{\link{attributes}} (other than a \code{source}
      attribute, see \code{\link{srcref}}), use \code{\link{structure}()}
      to display them as well as the object value unless the only such
      attribute is \code{names} and the \code{"niceNames"} option is set.
      This (\code{"showAttributes"}) is the default for
      \code{\link{deparse}} and \code{\link{dput}}.
    }
    \item{\code{"useSource"}:}{
      If the object has a \code{source} attribute (\code{\link{srcref}}),
      display that instead of deparsing the object.  Currently only
      applies to function definitions.
    }
    \item{\code{"warnIncomplete"}:}{
      Some exotic objects such as \link{environment}s, external
      pointers, etc. can not be deparsed properly.  This option causes a
      warning to be issued if the deparser recognizes one of these
      situations.

      Also, the parser in \R < 2.7.0 would only accept strings of up to
      8192 bytes, and this option gives a warning for longer strings.
    }
    \item{\code{"keepNA"}:}{
      Integer, real and character \code{NA}s are surrounded by coercion
      functions where necessary to ensure that they are parsed to the
      same type.  Since e.g.\sspace{}\code{NA_real_} can be output in \R, this is
      mainly used in connection with \code{S_compatible}.
    }
    \item{\code{"niceNames"}:}{
      If true, \code{\link{list}}s and atomic vectors with non-\code{\link{NA}}
      names (see \code{\link{names}}) are deparsed as e.g., \code{c(A = 1)}
      instead of \code{structure(1, .Names = "A")}, independently of the
      \code{"showAttributes"} setting.
    }
    \item{\code{"all"}:}{
      An abbreviated way to specify all of the options
      listed above.  This is the default for \code{dump}, and the options
      used by \code{\link{edit}} (which are fixed).
    }
    \item{\code{"delayPromises"}:}{
      Deparse promises in the form <promise: expression> rather than
      evaluating them.  The value and the environment of the promise
      will not be shown and the deparsed code cannot be sourced.
    }
    \item{\code{"S_compatible"}:}{
      Make deparsing as far as possible compatible with S and \R < 2.5.0.
      For compatibility with S, integer values of double vectors are
      deparsed with a trailing decimal point.  Backticks are not used.
    }
    \item{\code{"hexNumeric"}:}{
      Real and finite complex numbers are output in \samp{"\%a"} format as
      binary fractions (coded as hexadecimal: see \code{\link{sprintf}})
      with maximal opportunity to be recorded exactly to full precision.
      Complex numbers with one or both non-finite components are
      output as if this option were not set.

      (This relies on that format being correctly supported: known
      problems on Windows are worked around as from \R 3.1.2.)
    }
    \item{\code{"digits17"}:}{
      Real and finite complex numbers are output using format
      \samp{"\%.17g"} which may give more precision than the default
      (but the output will depend on the platform and there may be loss
      of precision when read back).  Complex numbers with one or both
      non-finite components are output as if this option were not set.
    }
  }
  For the most readable (but perhaps incomplete) display, use
  \code{control = NULL}.  This displays the object's value, but not its
  attributes.  The default in \code{\link{deparse}} is to display the
  attributes as well, but not to use any of the other options to make
  the result parseable.  (\code{\link{dput}} and \code{\link{dump}} do
  use more default options, and printing of functions without sources
  uses \code{c("keepInteger", "keepNA")}.)

  Using \code{control = c("all", "hexNumeric")} comes closest to making
  \code{deparse()} an inverse of \code{parse()}, as representing double
  and complex numbers as decimals may well not be exact.  However, not all
  objects are deparse-able even with this option.  A warning will be
  issued if the function recognizes that it is being asked to do the
  impossible.

  Only one of \code{"hexNumeric"} and \code{"digits17"} can be specified.
}
\value{
  An integer value corresponding to the \code{control} options
  selected.
}
\examples{
(iOpt.all <- .deparseOpts("all")) # a four digit integer

## one integer --> vector binary bits
int2bits <- function(x, base = 2L,
                     ndigits = 1 + floor(1e-9 + log(max(x,1), base))) {
    r <- numeric(ndigits)
    for (i in ndigits:1) {
        r[i] <- x\%\%base
        if (i > 1L)
            x <- x\%/\%base
    }
    rev(r) # smallest bit at left
}
int2bits(iOpt.all)
## what options does  "all" contain ?
(oa <- ..deparseOpts[-1][int2bits(iOpt.all) == 1])
stopifnot(identical(iOpt.all, .deparseOpts(oa)))
}
\keyword{programming}
