\name{print}
\title{Print Values}
\usage{
print(x, \dots)

\method{print}{factor}(x, quote = FALSE, max.levels = NULL,
      width = getOption("width"), \dots)

\method{print}{table}(x, digits = getOption("digits"), quote = FALSE,
      na.print = "", zero.print = "0", justify = "none", \dots)
}
\alias{print}
\alias{print.factor}
\alias{print.htest}
\alias{print.listof}
\alias{print.simple.list}
\alias{print.table}
\description{
  \code{print} prints its argument and returns it \emph{invisibly} (via
  \code{\link{invisible}(x)}).  It is a generic function which means that
  new printing methods can be easily added for new
  \code{\link[base]{class}}es.
}
\arguments{
  \item{x}{an object used to select a method.}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{quote}{logical, indicating whether or not strings should be
    printed with surrounding quotes.}
  \item{max.levels}{integer, indicating how many levels should be
    printed for a factor; if \code{0}, no extra "Levels" line will be
    printed.  The default, \code{NULL}, entails choosing \code{max.levels}
    such that the levels print on one line of width \code{width}.}
  \item{width}{only used when \code{max.levels} is NULL, see above.}
  \item{digits}{minimal number of \emph{significant} digits, see
    \code{\link{print.default}}.}
  \item{na.print}{character string (or \code{NULL}) indicating
    \code{\link{NA}} values in printed output, see
    \code{\link{print.default}}.}
  \item{zero.print}{character specifying how zeros (\code{0}) should be
    printed; for sparse tables, using \code{"."} can produce stronger results.}
  \item{justify}{character indicating if strings should left- or
    right-justified or left alone, passed to \code{\link{format}}.}
}
\details{
  The default method, \code{\link{print.default}} has its own help page.
  Use \code{\link{methods}("print")} to get all the methods for the
  \code{print} generic.

  \code{print.factor} allows some customization and is used for printing
  \code{\link{ordered}} factors as well.

  \code{print.table} for printing \code{\link{table}}s allows other
  customization.

  See \code{\link{noquote}} as an example of a class whose main
  purpose is a specific \code{print} method.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth \& Brooks/Cole.
}
\seealso{
  The default method \code{\link{print.default}}, and help for the
  methods above; further \code{\link{options}}, \code{\link{noquote}}.

  For more customizable (but cumbersome) printing, see
  \code{\link{cat}}, \code{\link{format}} or also \code{\link{write}}.
}
\examples{
require(stats)

ts(1:20)#-- print is the "Default function" --> print.ts(.) is called
rr <- for(i in 1:3) print(1:i)
rr

## Printing of factors
attenu$station ## 117 levels -> `max.levels' depending on width

## ordered factors: levels  "l1 < l2 < .."
esoph$agegp[1:12]
esoph$alcgp[1:12]

## Printing of sparse (contingency) tables
set.seed(521)
t1 <- round(abs(rt(200, df=1.8)))
t2 <- round(abs(rt(200, df=1.4)))
table(t1,t2) # simple
print(table(t1,t2), zero.print = ".")# nicer to read
}
\keyword{print}
