\name{print.default}
\title{Default Printing}
\usage{
print.default(x, digits = NULL, quote = TRUE, na.print = "NA",
              print.gap = 1, \dots)
}
\alias{print.default}
\description{
  \code{print} prints its argument.

  A non-null value for \code{digits} specifies the number of significant
  digits to be printed in values.  If \code{digits} is \code{NULL}, the
  value of \code{digits} set by \code{options} is used.

  The value of \code{quote} indicates whether or not strings should be
  printed with surrounding quotes.

  \code{na.print} is a string which is used to indicate \code{NA} values
  in printed output.

  \code{print.gap} gives the spacing between adjacent columns in printed
  matrices and arrays.
}
\seealso{
  \code{\link{options}}.
}
\examples{
pi
print(pi, digits = 16)
LETTERS[1:16]
print(LETTERS, quote = FALSE)
}
\keyword{print}
