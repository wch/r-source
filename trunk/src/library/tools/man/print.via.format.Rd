\name{.print.via.format}
\alias{.print.via.format}
\title{Printing Utilities}
\description{
  \code{.print.via.format} is a \dQuote{prototype} \code{\link{print}()}
  method, useful, at least as a start, by a simple
\preformatted{    print.<myS3class>  <-  .print.via.format}
}
\usage{
.print.via.format(x, ...)
}
\arguments{
  \item{x}{object to be printed.}
  \item{\dots}{optional further arguments, passed to \code{\link{format}}.}
}
\value{\code{x}, invisibly (by \code{\link{invisible}()}), as
  \code{\link{print}} methods should.
}
\seealso{
  The \code{\link{print}} generic; its default method
  \code{\link{print.default}} (used for many basic implicit classes such
  as \code{"numeric"}, \code{"character"} and \code{\link{array}}s of
  them, \code{\link{list}}s etc).
}
\examples{
## The function is simply defined as
 function (x, ...) {
    writeLines(format(x, ...))
    invisible(x)
 }

## is used for simple print methods in R, and as prototype for new methods.
}
\keyword{utilities}
