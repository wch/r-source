\name{format}
\title{Encode in a Common Format}
\usage{
format(x, \dots)
format.default(x, trim=FALSE, digits = .Options$digits)
format.pval(x, digits= max(1,.Options$digits-2), eps = .Machine$double.eps)
}
\alias{format}
\alias{format.default}
\alias{format.pval}
\arguments{
  \item{x}{any \R object (conceptually); typically numeric.}
  \item{trim}{logical; if \code{TRUE}, leading blanks are trimmed off the
    strings.}
  \item{digits}{how many significant digits are to be used for
    \code{\link{numeric} x}.}
}
\description{
These functions convert its first argument to a vector of character strings,
which have a common format (as is done by \code{\link{print}}).
The trimming with \code{trim = TRUE} is useful when the strings are to
be used for plot axis annotation.

\code{format.pval} is mainly an auxiliary function for
\code{\link{print.summary.lm}} etc., does separate formatting for
fixed, floating point and very small values (those \code{< eps}).

The function \code{\link{formatC}} provides a rather more flexible
formatting facility for numbers, does \emph{not} provide a common format
for several numbers, however.
}
\seealso{
\code{\link{formatC}}, \code{\link{paste}}, \code{\link{as.character}}.
}
\examples{
format(1:10)
for(i in c(1:5,10,15:17,20)) cat(i,":\t",format(pi,digits=i),"\n")

p _ c(47,13,2,.1,.023,.0045, 1e-100)/1000
format.pval(p)
format.pval(p / 0.9)
format.pval(p / 0.9, dig=3)
}
\keyword{character}
\keyword{print}
