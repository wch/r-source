\name{format.info}
\title{format(.) Information}
\usage{
format.info(x)
}
\alias{format.info}
\arguments{
 \item{x}{(numeric) vector; potential argument of
   \code{\link{format}(x,\dots)}.}
}
\description{
  Information is returned on how \code{\link{format}(x, digits =
    options("digits"))} would be formatted.
}
\value{
  An \code{\link{integer}} \code{\link{vector}} of length 3, say
  \code{r}.
  \item{r[1]}{width (number of characters) used for \code{format(x)}}
  \item{r[2]}{number of digits after decimal point.}
  \item{r[3]}{in \code{0:2}; if \eqn{\ge}{>=}\code{1}, \emph{exponential}
    representation would be used, with exponent length of \code{r[3]+1}.}
}
\note{The result \bold{depends} on the value of
  \code{\link{options}("digits")}.}
\seealso{\code{\link{format}}, \code{\link{formatC}}.}
\examples{
dd <- options("digits") ; options(digits = 7) #-- for the following
format.info(123) # 3 0 0
format.info(pi)  # 8 6 0
format.info(1e8) # 5 0 1 - exponential "1e+08"
format.info(1e222)#6 0 2 - exponential "1e+222"

x <- pi*10^c(-10,-2,0:2,8,20)
names(x) <- formatC(x,w=1,dig=3,format="g")
cbind(sapply(x,format))
t(sapply(x, format.info))

# Reset old options:
options(dd)
}
\keyword{character}
\keyword{print}
\keyword{programming}
