\name{format}
\title{Encode in a Common Format}
\usage{
format(x, \dots)
format.AsIs(x, width = 12, \dots)
format.data.frame(x, \dots, justify = "none") 
format.default(x, trim = FALSE, digits = getOption("digits"),
               justify = c("left", "right", "none"))
format.factor(x, \dots)
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
  \item{justify}{should character vector be left-justified,
    right-justified or left alone.  When justifying, the field width is
    that of the longest string.}
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

  \code{format.data.frame} formats the data frame column by column,
  applying the appropriate method of \code{format} for each column.
  The result is a data frame, so in most cases 
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

zz <- data.frame("(row names)"= c("aaaaa", "b"), check.names=FALSE)
format(zz)
format(zz, justify="left")

## handling of quotes
zz <- data.frame(a=I("abc"), b=I("def\"gh"))
format(zz)
\testonly{for(i in c(1:5,10,15:17,20)) cat(i,":\t",format(pi,digits=i),"\n")
}
p <- c(47,13,2,.1,.023,.0045, 1e-100)/1000
format.pval(p)
format.pval(p / 0.9)
format.pval(p / 0.9, dig=3)
}
\keyword{character}
\keyword{print}
