% File src/library/base/man/allnames.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{all.names}
\alias{all.names}
\alias{all.vars}
\title{Find All Names in an Expression}
\description{
  Return a character vector containing all the names which occur in an
  expression or call.
}
\usage{
all.names(expr, functions = TRUE, max.names = -1L, unique = FALSE)

all.vars(expr, functions = FALSE, max.names = -1L, unique = TRUE)
}
\arguments{
  \item{expr}{an \link{expression} or \link{call} from which the names
    are to be extracted.}
  \item{functions}{a logical value indicating whether function names
    should be included in the result.}
  \item{max.names}{the maximum number of names to be returned.  \code{-1}
    indicates no limit (other than vector size limits).}
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
\seealso{
  \code{\link{substitute}} to replace symbols with values in an expression.
}
\examples{
all.names(expression(sin(x+y)))
all.names(quote(sin(x+y))) # or a call
all.vars(expression(sin(x+y)))
}
\keyword{programming}
