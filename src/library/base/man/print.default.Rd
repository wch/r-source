\name{print.default}
\alias{print.default}
\alias{print.matrix}
\description{
  \code{print.default} is the \emph{default} method of the generic
  \code{\link{print}} function which prints its argument.
  \code{print.matrix} is currently identical, but was not prior to
  \code{1.7.0}.
}
\title{Default Printing}
\usage{
\method{print}{default}(x, digits = NULL, quote = TRUE, na.print = NULL,
      print.gap = NULL, right = FALSE, \dots)
}
\arguments{
  \item{x}{the object to be printed.}
  \item{digits}{a non-null value for \code{digits} specifies the minimum
    number of significant digits to be printed in values.  If
    \code{digits} is \code{NULL}, the value of \code{digits} set by
    \code{options} is used.}
  \item{quote}{logical, indicating whether or not strings
    (\code{\link{character}}s) should be printed with surrounding quotes.}
  \item{na.print}{a character string which is used to indicate
    \code{\link{NA}} values in printed output, or \code{NULL} (see Details)}
  \item{print.gap}{an integer, giving the spacing between adjacent columns
    in printed matrices and arrays, or \code{NULL} meaning 1.}
  \item{right}{logical, indicating whether or not strings should be
    right-aligned. The default is left-alignment.}
  \item{\dots}{further arguments to be passed to or from other
    methods. They are ignored in these functions.}
}
\details{
  Prior to \R 1.7.0, \code{print.matrix} did not
  print attributes and did not have a \code{digits} argument.
  
  The default for printing \code{NA}s is to print \code{NA} (without
  quotes) unless this is a character \code{NA} \emph{and} \code{quote =
    FALSE}, when \code{<NA>} is printed.

  The same number of decimal places is used throughout a vector,  This
  means that \code{digits} specifies the minimum number of significant
  digits to be used, and that at least one entry will be printed with
  that minimum number.

  As from \R 1.7.0 attributes are printed respecting their class(es),
  using the values of \code{digits} to \code{print.default}, but using the
  default values (for the methods called) of the other arguments.

  When the \pkg{methods} package is attached, \code{print} will call
  \code{\link{show}} for methods with formal classes if called
  with no optional arguments.
}
\seealso{The generic \code{\link{print}}, \code{\link{options}}.
  The \code{"\link{noquote}"} class and print method.
}
\examples{
pi
print(pi, digits = 16)
LETTERS[1:16]
print(LETTERS, quote = FALSE)
}
\keyword{print}
