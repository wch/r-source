\name{sprintf}
\alias{sprintf}
\title{Use C-style String Formatting Commands}
\description{
  A wrapper for the C function \code{sprintf}, that
  returns a character vector of length one containing a formatted
  combination of text and variable values.
}
\usage{
sprintf(fmt, \dots)
}
\arguments{
  \item{fmt}{a format string, which should be single-byte characters in
    a non-UTF-8 multibyte locale.}
  \item{\dots}{values to be passed into \code{fmt}.  Only logical,
    integer, real and character vectors are accepted, and only the first
    element is read from each vector.}
}
\details{
  This is a wrapper for the system \code{sprintf} C-library function.  
  Attempts are made to check that the mode of the values passed match
  the format supplied, and \R's special values (\code{NA}, \code{Inf},
  \code{-Inf} and \code{NaN}) are handled correctly.
  
  The following is abstracted from K&R (see References, below).  The
  string \code{fmt} contains normal characters, which are passed through
  to the output string, and also special characters that operate on the
  arguments provided through \code{\dots}.  Special characters start
  with a \code{\%} and terminate with one of the letters in the set
  \code{difeEgGsx\%}.  These letters denote the following types:

  \describe{
    \item{\code{d,i,x}}{Integer value, \code{x} being hexadecimal.}

    \item{\code{f}}{Double precision value, in decimal notation of the
      form "[-]mmm.ddd".  The number of decimal places is specified by
      the precision: the default is 6; a precision of 0 suppresses the
      decimal point.
    }
    
    \item{\code{e,E}}{Double precision value, in decimal notation of the
      form \code{[-]m.ddde[+-]xx} or \code{[-]m.dddE[+-]xx}.
    }

    \item{\code{g,G}}{Double precision value, in \code{\%e} or
      \code{\%E} format if the exponent is less than -4 or greater than or
      equal to the precision, and \code{\%f} format otherwise.
    }

    \item{\code{s}}{Character string.}

    \item{\code{\%}}{Literal \code{\%} (none of the formatting
      characters given below are permitted in this case).}
  }

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
  in order.

  The result has a length limit, probably 8192 characters, and attempts
  to exceed this may result in an error, or truncation with a warning.
}

\value{
  A character vector of length one.  Character \code{NA}s are converted
  to \code{"NA"}.
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
  \code{\link{formatC}} for a way of formatting vectors of
  numbers in a similar fashion.

  \code{\link{paste}} for another way of
  creating a vector combining text and values.
}

\examples{
% Escape all the '%' here !
## be careful with the format: most things in R are floats

sprintf("\%s is \%f feet tall\n", "Sven", 7) # OK
try(sprintf("\%s is \%i feet tall\n", "Sven", 7)) # not OK
sprintf("\%s is \%i feet tall\n", "Sven", as.integer(7)) # OK again

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
sprintf("\%-10f", pi)# left justified
sprintf("\%e", pi)
sprintf("\%E", pi)
sprintf("\%g", pi)
sprintf("\%g",   1e6 * pi) # -> exponential
sprintf("\%.9g", 1e6 * pi) # -> "fixed"
sprintf("\%G", 1e-6 * pi)

## no truncation:
sprintf("\%1.f",101)

## More sophisticated:

lapply(c("a", "ABC", "and an even longer one"),
       function(ch) sprintf("10-string '\%10s'", ch))

sapply(1:18, function(n)
       sprintf(paste("e with \%2d digits = \%.",n,"g",sep=""),
               n, exp(1)))

sprintf("second \%2$1.0f, first \%1$5.2f, third \%3$1.0f", pi, 2, 3)
}
\keyword{print}
\keyword{character}
