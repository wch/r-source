\name{print.default}
\title{Default Printing}
\usage{
print.default(x, digits = NULL, quote = TRUE, na.print = "NA",
              print.gap = 1, \dots)
print.atomic(x, quote = TRUE, \dots)
}
\alias{print.default}
\alias{print.atomic}
\arguments{
\item{digits}{a non-null value for \code{digits} specifies the number of
  significant digits to be printed in values.  If \code{digits} is
  \code{NULL}, the value of \code{digits} set by \code{options} is
  used.}
\item{quote}{logical, indicating whether or not strings
  (\code{\link{character}}s)  should be printed with surrounding quotes.}
\item{na.print}{a character string which is used to indicate
  \code{\link{NA}} values in printed output.}
\item{print.gap}{an integer, giving the spacing between adjacent columns
  in printed matrices and arrays.}
\item{\dots}{(further arguments, currently disregarded)}
}
\description{
  \code{print.default} is the \emph{default} method of the generic
  \code{\link{print}} function which prints its argument.

  \code{print.atomic} is almost the same and exists purely for
  compatibility reasons.
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
