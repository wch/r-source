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
  \item{fmt}{a format string.}
  \item{\dots}{values to be passed into \code{fmt}.  Only logical,
    integer, real and character vectors are supported, but some coercion
    will be done: see the Details section.}
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
  is done in parallel.
  
  The following is abstracted from Kernighan and Ritchie
  (see References).  The string \code{fmt} contains normal characters,
  which are passed through to the output string, and also special
  characters that operate on the arguments provided through
  \code{\dots}.  Special characters start with a \code{\%} and end with
  one of the letters in the set \code{difeEgGsxX\%}.  These letters
  denote the following types:

  \describe{
    \item{\code{d, i, x, X}}{Integer value, \code{x} and \code{X}
      being hexadecimal (using the same case for \code{a-f} as the code).
      Numeric variables with exactly integer values will be coerced to integer.
    }
    \item{\code{f}}{Double precision value, in decimal notation of the
      form "[-]mmm.ddd".  The number of decimal places is specified by
      the precision: the default is 6; a precision of 0 suppresses the
      decimal point.
    }
    \item{\code{e, E}}{Double precision value, in decimal notation of the
      form \code{[-]m.ddde[+-]xx} or \code{[-]m.dddE[+-]xx}.
    }
    \item{\code{g, G}}{Double precision value, in \code{\%e} or
      \code{\%E} format if the exponent is less than -4 or greater than or
      equal to the precision, and \code{\%f} format otherwise.
    }
    \item{\code{s}}{Character string.}
    \item{\code{\%}}{Literal \code{\%} (none of the formatting
      characters given below are permitted in this case).}
  }
  \code{\link{as.character}} is used for non-character arguments with
  \code{s} and \code{\link{as.double}} for non-double arguments with
  \code{f, e, E, g, G}.  NB: the length is determined before conversion,
  so do not rely on the internal coercion if this would change the
  length.
  
  In addition, between the initial \code{\%} and the terminating
  conversion character there may be, in any order:

  \describe{
    \item{\code{m.n}}{Two numbers separated by a period, denoting the
      field width (\code{m}) and the precision (\code{n})}
    \item{\code{-}}{Left adjustment of converted argument in its field}
    \item{\code{+}}{Always print number with sign}
    \item{a space}{Prefix a space if the first number is not a sign}
    \item{\code{0}}{For numbers, pad to the field width with leading zeros}
  }
  Further, as from \R 2.1.0, immediately after \code{\%} may come
  \code{1$} to \code{99$} to refer to the numbered argument: this allows
  arguments to referenced out of order and is mainly intended for
  translators of error messages.  If this is done it is best if all
  formats are numbered: if not the unnumbered ones process the arguments
  in order.  See the examples.

  The result has a length limit, probably 8192 bytes, and attempts
  to exceed this may result in an error, or truncation with a warning.
}

\value{
  A character vector of length that of the longest input.
  Character \code{NA}s are converted to \code{"NA"}.
}

\references{
  Kernighan, B. W. and Ritchie, D. M. (1988)
  \emph{The C Programming Language.} Second edition, Prentice Hall.
  describes the format options in table B-1 in the Appendix.
}

\author{
  Original code by Jonathan Rougier, \email{J.C.Rougier@durham.ac.uk}.
}

\seealso{
  \code{\link{formatC}} for a way of formatting vectors of numbers in a
  similar fashion.

  \code{\link{paste}} for another way of creating a vector combining
  text and values.

  \code{\link{gettext}} for the mechanisms for the automated translation
  of text.
}

\examples{
% Escape all the '%' here !
## be careful with the format: most things in R are floats
## only integer-valued reals get coerced to integer.

sprintf("\%s is \%f feet tall\n", "Sven", 7.1)      # OK
try(sprintf("\%s is \%i feet tall\n", "Sven", 7.1)) # not OK
try(sprintf("\%s is \%i feet tall\n", "Sven", 7))   # OK

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
sprintf("\%1.f",101)

## re-use one argument three times, show difference between \%x and \%X
xx <- sprintf("\%1$d \%1$x \%1$X", 0:15)
xx <- matrix(xx, dimnames=list(rep("", 16), "\%d\%x\%X"))
noquote(format(xx, justify="right"))

## More sophisticated:

sprintf("min 10-char string '\%10s'",
        c("a", "ABC", "and an even longer one"))

n <- 1:18
sprintf(paste("e with \%2d digits = \%.",n,"g",sep=""), n, exp(1))

## Using arguments out of order
sprintf("second \%2$1.0f, first \%1$5.2f, third \%3$1.0f", pi, 2, 3)

## re-cycle arguments 
sprintf("\%s \%d", "test", 1:3)
}
\keyword{print}
\keyword{character}
