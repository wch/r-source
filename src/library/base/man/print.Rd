\name{print}
\title{Print Values}
\usage{
print(x, \dots)

print.factor(x, quote = FALSE, max.levels = NULL,
             width = getOption("width"), \dots)
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
    printed.  The default, \code{NULL}, entails chosing \code{max.levels}
    such that the levels print on one line of width \code{width}.}
  \item{width}{only used when \code{max.levels} is NULL, see above.}
}
\details{
  The default method, \code{\link{print.default}} has its own help page.
  Use \code{\link{methods}("print")} to get all the methods for the
  \code{print} generic.

  \code{print.factor} allows some customization and is used for printing
  \code{\link{ordered}} factors as well.

  See \code{\link{noquote}} as an example of a class whose main
  purpose is a specific \code{print} method.
}
\seealso{
  The default method \code{\link{print.default}}, and help for the
  methods above; further \code{\link{options}}, \code{\link{noquote}}.
}
\examples{
ts(1:20)#-- print  is the ``Default function'' --> print.ts(.) is called
rr <- for(i in 1:3) print(1:i)
rr

## Printing of factors illustrated for ex

}
\keyword{print}
