\name{name}
\alias{name}
\alias{is.symbol}
\alias{as.symbol}
\alias{as.name}
\alias{is.name}
\title{Variable Names or Symbols, respectively}
\usage{
as.symbol(x)
is.symbol(y)

as.name(x)
is.name(y)
}
\description{
  \code{as.symbol} coerces its argument to be a \emph{symbol}, or
  equivalently, a \emph{name}.  The argument must
  be of mode \code{"character"}.  \code{as.name} is an
  alias for \code{as.symbol}.

  \code{is.symbol} (and \code{is.name} equivalently) returns \code{TRUE}
  or \code{FALSE} depending on whether its argument is a symbol
  (i.e. name) or not.
}
\arguments{
  \item{x, y}{objects to be coerced or tested.} 
}
\note{
  The term ``symbol'' is from the lisp background of \R, whereas ``name''
  has been the standard S term for this.
}
\details{
  \code{is.symbol} is generic: you can write methods to handle
  of specific classes of objects, see \link{InternalMethods}.
}
\seealso{
  \code{\link{call}}, \code{\link{is.language}}. For the internal
  object mode, \code{\link{typeof}}.
}
\examples{
an <- as.name("arrg")
is.name(an) # TRUE
str(an)# symbol
}
\keyword{programming}
\keyword{attribute}
