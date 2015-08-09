% File src/library/base/man/sprintf.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{sprintf}
\alias{sprintf}
\alias{gettextf}
\title{Use C-style String Formatting Commands}
\description{
  A wrapper for the C function \code{sprintf}, that returns a character
  vector containing a formatted combination of text and variable values.
}
\usage{
sprintf(fmt, \dots)
gettextf(fmt, \dots, domain = NULL)
}
\arguments{
  \item{fmt}{a character vector of format strings, each of up to 8192 bytes.}
  \item{\dots}{values to be passed into \code{fmt}.  Only logical,
    integer, real and character vectors are supported, but some coercion
    will be done: see the \sQuote{Details} section.}
  \item{domain}{see \code{\link{gettext}}.}
}
\details{
  \code{sprintf} is a wrapper for the system \code{sprintf} C-library
  function.  Attempts are made to check that the mode of the values
  passed match the format supplied, and \R's special values (\code{NA},
  \code{Inf}, \code{-Inf} and \code{NaN}) are handled correctly.

  \code{gettextf} is a convenience function which provides C-style
  string formatting with possible translation of the format string.

  The arguments (including \code{fmt}) are recycled if possible a whole
  number of times to the length of the longest, and then the formatting
  is done in parallel.  Zero-length arguments are allowed and will give
  a zero-length result.  All arguments are evaluated even if unused, and
  hence some types (e.g., \code{"symbol"} or \code{"language"}, see
  \code{\link{typeof}}) are not allowed.

  The following is abstracted from Kernighan and Ritchie (see
  References): however the actual implementation will follow the C99
  standard and fine details (especially the behaviour under user error)
  may depend on the platform.

  The string \code{fmt} contains normal characters,
  which are passed through to the output string, and also conversion
  specifications which operate on the arguments provided through
  \code{\dots}.  The allowed conversion specifications start with a
  \code{\%} and end with one of the letters in the set
  \code{aAdifeEgGosxX\%}.  These letters denote the following types:

  \describe{
    \item{\code{d}, \code{i}, \code{o}, \code{x}, \code{X}}{Integer
      value, \code{o} being octal, 
      \code{x} and \code{X} being hexadecimal (using the same case for
      \code{a-f} as the code).  Numeric variables with exactly integer
      values will be coerced to integer.  Formats \code{d} and \code{i}
      can also be used for logical variables, which will be converted to
      \code{0}, \code{1} or \code{NA}.
    }
    \item{\code{f}}{Double precision value, in \dQuote{\bold{f}ixed
        point} decimal notation of the form "[-]mmm.ddd".  The number of
      decimal places ("d") is specified by the precision: the default is 6;
      a precision of 0 suppresses the decimal point.  Non-finite values
      are converted to \code{NA}, \code{NaN} or (perhaps a sign followed
      by) \code{Inf}.
    }
    \item{\code{e}, \code{E}}{Double precision value, in
      \dQuote{\bold{e}xponential} decimal notation of the
      form \code{[-]m.ddde[+-]xx} or \code{[-]m.dddE[+-]xx}.
    }
    \item{\code{g}, \code{G}}{Double precision value, in \code{\%e} or
      \code{\%E} format if the exponent is less than -4 or greater than or
      equal to the precision, and \code{\%f} format otherwise.
      (The precision (default 6) specifies the number of
      \emph{significant} digits here, whereas in \code{\%f, \%e}, it is
      the number of digits after the decimal point.)
    }
    \item{\code{a}, \code{A}}{Double precision value, in binary notation
      of the form \code{[-]0xh.hhhp[+-]d}.  This is a binary fraction
      expressed in hex multiplied by a (decimal) power of 2.  The number
      of hex digits after the decimal point is specified by the precision:
      the default is enough digits to represent exactly the internal
      binary representation.  Non-finite values are converted to \code{NA},
      \code{NaN} or (perhaps a sign followed by) \code{Inf}.  Format
      \code{\%a} uses lower-case for \code{x}, \code{p} and the hex
      values: format \code{\%A} uses upper-case.

      This should be supported on all platforms as it is a feature of C99.
      The format is not uniquely defined: although it would be possible
      to make the leading \code{h} always zero or one, this is not
      always done.  Most systems will suppress trailing zeros, but a few
      do not.  On a well-written platform, for normal numbers there will
      be a leading one before the decimal point plus (by default) 13
      hexadecimal digits, hence 53 bits.  The treatment of denormalized
      (aka \sQuote{subnormal}) numbers is very platform-dependent.
    }
    \item{\code{s}}{Character string.  Character \code{NA}s are
      converted to \code{"NA"}.
    }
    \item{\code{\%}}{Literal \code{\%} (none of the extra formatting
      characters given below are permitted in this case).
    }
  }
  Conversion by \code{\link{as.character}} is used for non-character
  arguments with \code{s} and by \code{\link{as.double}} for
  non-double arguments with \code{f, e, E, g, G}.  NB: the length is
  determined before conversion, so do not rely on the internal
  coercion if this would change the length.  The coercion is done only
  once, so if \code{length(fmt) > 1} then all elements must expect the
  same types of arguments.

  In addition, between the initial \code{\%} and the terminating
  conversion character there may be, in any order:

  \describe{
    \item{\code{m.n}}{Two numbers separated by a period, denoting the
      field width (\code{m}) and the precision (\code{n}).}
    \item{\code{-}}{Left adjustment of converted argument in its field.}
    \item{\code{+}}{Always print number with sign: by default only
      negative numbers are printed with a sign.}
    \item{a space}{Prefix a space if the first character is not a sign.}
    \item{\code{0}}{For numbers, pad to the field width with leading
      zeros. For characters, this zero-pads on some platforms and is
      ignored on others.}
    \item{\code{#}}{specifies \dQuote{alternate output} for numbers, its
      action depending on the type:
      For \code{x} or \code{X}, \code{0x} or \code{0X} will be prefixed
      to a non-zero result.  For \code{e}, \code{e}, \code{f}, \code{g}
      and \code{G}, the output will always have a decimal point; for
      \code{g} and \code{G}, trailing zeros will not be removed.
    }
  }
  Further, immediately after \code{\%} may come \code{1$} to \code{99$}
  to refer to numbered argument: this allows arguments to be
  referenced out of order and is mainly intended for translators of
  error messages.  If this is done it is best if all formats are
  numbered: if not the unnumbered ones process the arguments in order.
  See the examples.  This notation allows arguments to be used more than
  once, in which case they must be used as the same type (integer,
  double or character).

  A field width or precision (but not both) may be indicated by an
  asterisk \code{*}: in this case an argument specifies the desired
  number.  A negative field width is taken as a '-' flag followed by a
  positive field width.  A negative precision is treated as if the
  precision were omitted.  The argument should be integer, but a double
  argument will be coerced to integer.

  There is a limit of 8192 bytes on elements of \code{fmt}, and on
  strings included from a single \code{\%}\emph{letter} conversion
  specification.

  Field widths and precisions of \code{\%s} conversions are interpreted
  as bytes, not characters, as described in the C standard.
  
  The C doubles used for \R numerical vectors have signed zeros, which
  \code{sprintf} may output as \code{-0}, \code{-0.000} \dots.
}
%% (for #) This is from Kernighan & Ritchie, p. 243:
%% For \code{o}, the first digit will be zero.


\value{
  A character vector of length that of the longest input.  If any
  element of \code{fmt} or any character argument is declared as UTF-8,
  the element of the result will be in UTF-8 and have the encoding
  declared as UTF-8.  Otherwise it will be in the current locale's
  encoding.
}

\section{Warning}{
  The format string is passed down the OS's \code{sprintf} function, and
  incorrect formats can cause the latter to crash the \R process .  \R
  does perform sanity checks on the format, but not all possible user
  errors on all platforms have been tested, and some might be terminal.

  The behaviour on inputs not documented here is \sQuote{undefined},
  which means it is allowed to differ by platform.
}

\references{
  Kernighan, B. W. and Ritchie, D. M. (1988)
  \emph{The C Programming Language.} Second edition, Prentice Hall.
  Describes the format options in table B-1 in the Appendix.

  The C Standards, especially ISO/IEC 9899:1999 for \sQuote{C99}.  Links
  can be found at \url{http://developer.r-project.org/Portability.html}.

  \command{man sprintf} on a Unix-alike system.
}

\author{
  Original code by Jonathan Rougier.
}

\seealso{
  \code{\link{formatC}} for a way of formatting vectors of numbers in a
  similar fashion.

  \code{\link{paste}} for another way of creating a vector combining
  text and values.

  \code{\link{gettext}} for the mechanisms for the automated translation
  of text.
}

% Escape all the '%' here !
\examples{
## be careful with the format: most things in R are floats
## only integer-valued reals get coerced to integer.

sprintf("\%s is \%f feet tall\n", "Sven", 7.1)      # OK
try(sprintf("\%s is \%i feet tall\n", "Sven", 7.1)) # not OK
    sprintf("\%s is \%i feet tall\n", "Sven", 7  )  # OK

## use a literal \% :

sprintf("\%.0f\%\% said yes (out of a sample of size \%.0f)", 66.666, 3)

## various formats of pi :

sprintf("\%f", pi)
sprintf("\%.3f", pi)
sprintf("\%1.0f", pi)
sprintf("\%5.1f", pi)
sprintf("\%05.1f", pi)
sprintf("\%+f", pi)
sprintf("\% f", pi)
sprintf("\%-10f", pi) # left justified
sprintf("\%e", pi)
sprintf("\%E", pi)
sprintf("\%g", pi)
sprintf("\%g",   1e6 * pi) # -> exponential
sprintf("\%.9g", 1e6 * pi) # -> "fixed"
sprintf("\%G", 1e-6 * pi)

## no truncation:
sprintf("\%1.f", 101)

## re-use one argument three times, show difference between \%x and \%X
xx <- sprintf("\%1$d \%1$x \%1$X", 0:15)
xx <- matrix(xx, dimnames = list(rep("", 16), "\%d\%x\%X"))
noquote(format(xx, justify = "right"))

## More sophisticated:

sprintf("min 10-char string '\%10s'",
        c("a", "ABC", "and an even longer one"))

\donttest{## Platform-dependent bad example from qdapTools 1.0.0:
## may pad with spaces or zeroes.
sprintf("\%09s", month.name)}

n <- 1:18
sprintf(paste0("e with \%2d digits = \%.", n, "g"), n, exp(1))

## Using arguments out of order
sprintf("second \%2$1.0f, first \%1$5.2f, third \%3$1.0f", pi, 2, 3)

## Using asterisk for width or precision
sprintf("precision \%.*f, width '\%*.3f'", 3, pi, 8, pi)

## Asterisk and argument re-use, 'e' example reiterated:
sprintf("e with \%1$2d digits = \%2$.*1$g", n, exp(1))

## re-cycle arguments
sprintf("\%s \%d", "test", 1:3)

## binary output showing rounding/representation errors
x <- seq(0, 1.0, 0.1); y <- c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1)
cbind(x, sprintf("\%a", x), sprintf("\%a", y))
}
\keyword{print}
\keyword{character}
