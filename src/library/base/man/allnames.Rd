\name{all.names}
\title{Find All Names in an Expression}
\usage{
all.names(expr, functions = T,
        max.names = 200, unique = F)
all.vars(expr, functions = F,
        max.names = 200, unique = T)
}
\alias{all.names}
\alias{all.vars}
\arguments{
  \item{expr}{an expression or call from which the names are to be extracted.}
  \item{functions}{a logical value indicating whether function names should
        be included in the result.}
  \item{max.names}{the maximum number of names to be returned.}
  \item{unique}{a logical value which indicates whether duplicate names
        should be removed from the value.}
}

\value{
These functions return a character vector containing
all the names which occur in an expression or call.
These functions differ only in the default values for their arguments.
}
\examples{
all.names(expression(sin(x+y)))
all.vars(expression(sin(x+y)))
}
\keyword{programming}
