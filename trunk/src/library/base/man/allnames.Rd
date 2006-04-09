\name{all.names}
\alias{all.names}
\alias{all.vars}
\title{Find All Names in an Expression}
\description{
  Return a character vector containing all the names which occur in an
  expression or call.
}
\usage{
all.names(expr, functions = TRUE, max.names = 200, unique = FALSE)

all.vars(expr, functions = FALSE, max.names = 200, unique = TRUE)
}
\arguments{
  \item{expr}{an expression or call from which the names are to be
    extracted.}
  \item{functions}{a logical value indicating whether function names
    should be included in the result.}
  \item{max.names}{the maximum number of names to be returned.}
  \item{unique}{a logical value which indicates whether duplicate names
    should be removed from the value.}
}
\details{
  These functions differ only in the default values for their
  arguments.
}
\value{
  A character vector with the extracted names.
}
\examples{
all.names(expression(sin(x+y)))
all.vars(expression(sin(x+y)))
}
\keyword{programming}
