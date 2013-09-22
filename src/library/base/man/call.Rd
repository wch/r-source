% File src/library/base/man/call.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{call}
\alias{call}
\alias{is.call}
\alias{as.call}
\title{Function Calls}
\description{
  Create or test for objects of mode \code{"call"}.
}
\usage{
call(name, \dots)
is.call(x)
as.call(x)
}
\arguments{
  \item{name}{a non-empty character string naming the function to be called.}
  \item{\dots}{arguments to be part of the call.}
  \item{x}{an arbitrary \R object.}
}
\details{
  \code{call} returns an unevaluated function call, that is, an
  unevaluated expression which consists of the named function applied to
  the given arguments (\code{name} must be a quoted string which gives
  the name of a function to be called).  Note that although the call is
  unevaluated, the arguments \code{\dots} are evaluated.

  \code{call} is a primitive, so the first argument is
  taken as \code{name} and the remaining arguments as arguments for the
  constructed call: if the first argument is named the name must
  partially match \code{name}.

  \code{is.call} is used to determine whether \code{x} is a call (i.e.,
  of mode \code{"call"}).

  Objects of mode \code{"list"} can be coerced to mode \code{"call"}.
  The first element of the list becomes the function part of the call,
  so should be a function or the name of one (as a symbol; a quoted
  string will not do).

  All three are \link{primitive} functions. 
}
\section{Warning}{
  \code{call} should not be used to attempt to evade restrictions on the
  use of \code{.Internal} and other non-API calls.
}
\seealso{
  \code{\link{do.call}} for calling a function by name and argument
  list;
  \code{\link{Recall}} for recursive calling of functions;
  further
  \code{\link{is.language}},
  \code{\link{expression}},
  \code{\link{function}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
is.call(call) #-> FALSE: Functions are NOT calls

## set up a function call to round with argument 10.5
cl <- call("round", 10.5)
is.call(cl) # TRUE
cl
## such a call can also be evaluated.
eval(cl) # [1] 10

A <- 10.5
call("round", A)        # round(10.5)
call("round", quote(A)) # round(A)
f <- "round"
call(f, quote(A))       # round(A)
## if we want to supply a function we need to use as.call or similar
f <- round
\dontrun{call(f, quote(A))  # error: first arg must be character}
(g <- as.call(list(f, quote(A))))
eval(g)
## alternatively but less transparently
g <- list(f, quote(A))
mode(g) <- "call"
g
eval(g)
## see also the examples in the help for do.call
}
\keyword{programming}
\keyword{attribute}
