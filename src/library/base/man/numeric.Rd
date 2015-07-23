% File src/library/base/man/numeric.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{numeric}
\title{Numeric Vectors}
\alias{numeric}
\alias{as.numeric}
\alias{is.numeric}
\alias{is.numeric.Date}
\alias{is.numeric.POSIXt}
\description{
  Creates or coerces objects of type \code{"numeric"}.
  \code{is.numeric} is a more general test of an object being
  interpretable as numbers.
}
\usage{
numeric(length = 0)
as.numeric(x, \dots)
is.numeric(x)
}
\arguments{
  \item{length}{A non-negative integer specifying the desired length.
    Double values will be coerced to integer:
    supplying an argument of length other than one is an error.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{numeric} is identical to \code{\link{double}} (and \code{real}).
  It creates a double-precision vector of the specified length with each
  element equal to \code{0}.

  \code{as.numeric} is a generic function, but S3 methods must be
  written for \code{\link{as.double}}.  It is identical to \code{as.double}.

  \code{is.numeric} is an \link{internal generic} \code{primitive}
  function: you can write methods to handle specific classes of objects,
  see \link{InternalMethods}.  It is \strong{not} the same as
  \code{\link{is.double}}.  Factors are handled by the default method,
  and there are methods for classes \code{"\link{Date}"},
  \code{"\link{POSIXt}"} and \code{"\link{difftime}"} (all of which
  return false).  Methods for \code{is.numeric} should only return true
  if the base type of the class is \code{double} or \code{integer}
  \emph{and} values can reasonably be regarded as numeric
  (e.g., arithmetic on them makes sense, and comparison should be done
  via the base type).
}
\value{
  for \code{numeric} and \code{as.numeric} see \code{\link{double}}.

  The default method for \code{is.numeric} returns \code{TRUE}
  if its argument is of \link{mode} \code{"numeric"}
  (\link{type} \code{"double"} or type \code{"integer"}) and not a
  factor, and \code{FALSE} otherwise.  That is,
  \code{is.integer(x) || is.double(x)}, or
  \code{(mode(x) == "numeric") && !is.factor(x)}.
}
\section{S4 methods}{
  \code{as.numeric} and \code{is.numeric} are internally S4 generic and
  so methods can be set for them \emph{via} \code{setMethod}.

  To ensure that \code{as.numeric} and \code{as.double}
  remain identical, S4 methods can only be set for \code{as.numeric}.
}
%% keep next the same in double.Rd & numeric.Rd
\section{Note on names}{
  It is a historical anomaly that \R has two names for its
  floating-point vectors, \code{\link{double}} and \code{\link{numeric}}
  (and formerly had \code{real}).

  \code{double} is the name of the \link{type}.
  \code{numeric} is the name of the \link{mode} and also of the implicit
  \link{class}.  As an S4 formal class, use \code{"numeric"}.

  The potential confusion is that \R has used \emph{\link{mode}}
  \code{"numeric"} to mean \sQuote{double or integer}, which conflicts
  with the S4 usage.  Thus \code{is.numeric} tests the mode, not the
  class, but \code{as.numeric} (which is identical to \code{as.double})
  coerces to the class.
}
\seealso{
  \code{\link{double}}, \code{\link{integer}}, \code{\link{storage.mode}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
as.numeric(c("-.1"," 2.7 ","B")) # (-0.1, 2.7, NA)  +  warning
as.numeric(factor(5:10))
}
\keyword{classes}
\keyword{attribute}
