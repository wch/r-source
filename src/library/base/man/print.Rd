\name{print}
\title{Print Values}
\usage{
print(x, \dots)

print.factor(x, quote = FALSE)
print.ordered(x, quote = FALSE)
}
\alias{print}
\alias{print.factor}
\alias{print.ordered}
\alias{print.htest}
\alias{print.listof}
\alias{print.table}
\alias{print.tabular}
\description{
\code{print} prints its argument.
It is a generic function which means that new printing methods can be
easily added for new \code{\link{class}}es.

The default method, \code{\link{print.default}} has its own help page.
Use \code{\link{methods}("print")} to get all the methods for the
\code{print} generic.
}
\seealso{The default method \code{\link{print.default}}, and help for the
  methods above; further \code{\link{options}}.
}
\examples{
ts(1:20)#-- print  is the ``Default function'' --> print.ts(.) is called
rr <- for(i in 1:3) print(1:i)
rr
}
\keyword{print}
