\name{format}
\title{Encode in a Common Format}
\usage{
format(x, \dots)
format.AsIs(x, width = 12, \dots)
format.data.frame <- function(x, \dots) 
format.default(x, trim = FALSE, digits = getOption("digits"))
format.factor <- function(x, \dots)
format.pval(pv, digits = max(1, getOption("digits") - 2),
            eps = .Machine$double.eps, na.form = "NA")
}
\alias{format}
\alias{format.AsIs}
\alias{format.data.frame}
\alias{format.default}
\alias{format.factor}
\alias{format.pval}
\description{
  Format an \R object for pretty printing: \code{format.pval} is
  intended for formatting p-values.
}
\arguments{
  \item{x}{any \R object (conceptually); typically numeric.}
  \item{trim}{logical; if \code{TRUE}, leading blanks are trimmed off the
    strings.}
  \item{digits}{how many significant digits are to be used for
    \code{\link{numeric} x}. This is a suggestion: enough decimal
    places will be used so that the smallest (in magnitude) number has
    this many significant digits.}
  \item{pv}{a numeric vector.}
  \item{na.form}{character representation of \code{NA}s.}
  \item{width}{the returned vector has elements of at most \code{width}.}
}
\details{
  These functions convert their first argument to a vector (or array) of
  character strings which have a common format (as is done by
  \code{\link{print}}), fulfilling \code{length(format*(x, *)) == 
    length(x)}.  The trimming with \code{trim = TRUE} is useful
  when the strings are to be used for plot \code{\link{axis}} annotation.

  \code{format.AsIs} deals with columns of complicated objects that
  have been extracted from a data frame.

  \code{format.pval} is mainly an auxiliary function for
  \code{\link{print.summary.lm}} etc., does separate formatting for
  fixed, floating point and very small values (those \code{< eps}).

  The function \code{\link{formatC}} provides a rather more flexible
  formatting facility for numbers, but does \emph{not} provide a common
  format for several numbers.

}
\note{
  Currently \code{format} drops trailing zeroes, so \code{format(6.001,
    digits=2)} gives \code{"6"} and \code{format(c(6.0, 13.1),
    digits=2)} gives \code{c(" 6", "13")}.

  Character(s) \code{"} in input strings \code{x} are escaped to \code{\\"}.
}

\seealso{
  \code{\link{formatC}}, \code{\link{paste}}, \code{\link{as.character}}.
}
\examples{
format(1:10)
\testonly{for(i in c(1:5,10,15:17,20)) cat(i,":\t",format(pi,digits=i),"\n")
}
p <- c(47,13,2,.1,.023,.0045, 1e-100)/1000
format.pval(p)
format.pval(p / 0.9)
format.pval(p / 0.9, dig=3)
}
\keyword{character}
\keyword{print}
