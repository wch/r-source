% File src/library/base/man/name.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{name}
\alias{name}
\concept{symbol}%\alias{symbol} use for plotmath as from R 2.6.0
\alias{is.symbol}
\alias{as.symbol}
\alias{as.name}
\alias{is.name}
\title{Names and Symbols}
\description{
  A \sQuote{name} (also known as a \sQuote{symbol}) is a way to refer to
  \R objects by name (rather than the value of the object, if any, bound
  to that name).

  \code{as.name} and \code{as.symbol} are identical: they attempt to
  coerce the argument to a name.

  \code{is.symbol} and the identical \code{is.name} return \code{TRUE}
  or \code{FALSE} depending on whether the argument is a name or not.
}
\usage{
as.symbol(x)
is.symbol(x)

as.name(x)
is.name(x)
}
\arguments{
  \item{x}{object to be coerced or tested.}
}
\details{
  Names are limited to 10,000 bytes (and were to 256 bytes in versions
  of \R before 2.13.0).

  \code{as.name} first coerces its argument internally to a character
  vector (so methods for \code{as.character} are not used).  It then
  takes the first element and provided it is not \code{""}, returns a
  symbol of that name (and if the element is \code{NA_character_}, the
  name is \code{`NA`}).

  \code{as.name} is implemented as \code{\link{as.vector}(x, "symbol")},
  and hence will dispatch methods for the generic function \code{as.vector}.

  \code{is.name} and \code{is.symbol} are \link{primitive} functions.
}
\value{
  For \code{as.name} and \code{as.symbol}, an \R object of type
  \code{"symbol"} (see \code{\link{typeof}}).

  For \code{is.name} and \code{is.symbol}, a length-one logical vector
  with value \code{TRUE} or \code{FALSE}.
}
\note{
  The term \sQuote{symbol} is from the LISP background of \R, whereas
  \sQuote{name} has been the standard S term for this.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{call}}, \code{\link{is.language}}.
  For the internal object mode, \code{\link{typeof}}.

  \code{\link{plotmath}} for another use of \sQuote{symbol}.
}
\examples{
an <- as.name("arrg")
is.name(an) # TRUE
mode(an)   # name
typeof(an) # symbol
}
\keyword{programming}
\keyword{attribute}
