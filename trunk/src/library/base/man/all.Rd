% File src/library/base/man/all.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2010 R Core Team
% Distributed under GPL 2 or later

\name{all}
\title{Are All Values True?}
\usage{
all(\dots, na.rm = FALSE)
}
\alias{all}
\description{
  Given a set of logical vectors, are all of the values true?
}
\arguments{
  \item{\dots}{zero or more logical vectors.  Other objects of zero
    length are ignored, and the rest are coerced to logical ignoring
    any class.}
  \item{na.rm}{logical.  If true \code{NA} values are removed before
    the result is computed.}
}
\details{
  This is a generic function: methods can be defined for it
  directly or via the \code{\link[=S3groupGeneric]{Summary}} group generic.
  For this to work properly, the arguments \code{\dots} should be
  unnamed, and dispatch is on the first argument.

  Coercion of types other than integer (raw, double, complex, character,
  list) gives a warning as this is often unintentional.

  This is a \link{primitive} function.
}
\value{
  The value is a logical vector of length one.

  Let \code{x} denote the concatenation of all the logical vectors in
  \code{...} (after coercion), after removing \code{NA}s if requested by
  \code{na.rm = TRUE}.

  The value returned is \code{TRUE} if all of the values in \code{x} are
  \code{TRUE} (including if there are no values), and \code{FALSE} if at
  least one of the values in \code{x} is \code{FALSE}.  Otherwise the
  value is \code{NA} (which can only occur if \code{na.rm = FALSE} and
  \code{\dots} contains no \code{FALSE} values and at least one
  \code{NA} value).
}
\note{
  That \code{all(logical(0))} is true is a useful convention:
  it ensures that
\preformatted{all(all(x), all(y)) == all(x, y)
}
  even if \code{x} has length zero.
}
\section{S4 methods}{
  This is part of the S4 \code{\link[=S4groupGeneric]{Summary}}
  group generic.  Methods for it must use the signature
  \code{x, \dots, na.rm}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{any}}, the \sQuote{complement} of \code{all}, and
  \code{\link{stopifnot}(*)} which is an \code{all(*)}
  \sQuote{insurance}.
}
\examples{
range(x <- sort(round(stats::rnorm(10) - 1.2, 1)))
if(all(x < 0)) cat("all x values are negative\n")

all(logical(0))  # true, as all zero of the elements are true.
}
\keyword{logic}
