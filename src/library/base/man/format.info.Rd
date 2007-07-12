\name{format.info}
\alias{format.info}
\title{format(.) Information}
\description{
  Information is returned on how \code{\link{format}(x, digits, nsmall)}
  would be formatted.
}
\usage{
format.info(x, digits = NULL, nsmall = 0)
}
\arguments{
 \item{x}{an atomic vector; a potential argument of
   \code{\link{format}(x, \dots)}.}
  \item{digits}{how many significant digits are to be used for
    numeric and complex \code{x}.  The default, \code{NULL}, uses
    \code{\link{getOption}(digits)}.}
  \item{nsmall}{ (see \code{\link{format}(\dots, nsmall)}).}
}

\value{
  An \code{\link{integer}} \code{\link{vector}} of length 1, 3 or 6, say
  \code{r}.

  For logical, integer and character vectors a single element,
  the width which would be used by \code{format} if \code{width = NULL}.

  For numeric vectors:
  \item{r[1]}{width (in characters) used by \code{format(x)}}
  \item{r[2]}{number of digits after decimal point.}
  \item{r[3]}{in \code{0:2}; if \eqn{\ge}{>=}\code{1}, \emph{exponential}
    representation would be used, with exponent length of \code{r[3]+1}.}

  For a complex vector the first three elements refer to the real parts,
  and there are three further elements corresponding to the imaginary
  parts.
}
\seealso{
  \code{\link{format}}, \code{\link{formatC}}.
}
\examples{
dd <- options("digits") ; options(digits = 7) #-- for the following
format.info(123)   # 3 0 0
format.info(pi)    # 8 6 0
format.info(1e8)   # 5 0 1 - exponential "1e+08"
format.info(1e222) # 6 0 2 - exponential "1e+222"

x <- pi*10^c(-10,-2,0:2,8,20)
names(x) <- formatC(x, width=1, digits=3, format="g")
cbind(sapply(x,format))
t(sapply(x, format.info))

## using at least 8 digits right of "."
t(sapply(x, format.info, nsmall = 8))

# Reset old options:
options(dd)
}
\keyword{character}
\keyword{print}
\keyword{programming}
