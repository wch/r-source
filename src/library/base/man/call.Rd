% File src/library/base/man/call.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{call}
\title{Function Calls}
\alias{call}
\alias{is.call}
\alias{as.call}
\description{
  Create or test for objects of \code{\link{mode}} \code{"call"} (or
  \code{"("}, see Details).
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
  \describe{
    \item{\code{call} }{returns an unevaluated function call, that is, an
      unevaluated expression which consists of the named function applied to
      the given arguments (\code{name} must be a quoted string which gives
      the name of a function to be called).  Note that although the call is
      unevaluated, the arguments \code{\dots} are evaluated.

      \code{call} is a primitive, so the first argument is
      taken as \code{name} and the remaining arguments as arguments for the
      constructed call: if the first argument is named the name must
      partially match \code{name}.
    }
    \item{\code{is.call} }{is used to determine whether \code{x} is a call (i.e.,
      of mode \code{"call"} or \code{"("}).  Note that
      \itemize{
	\item{\code{is.call(x)} is strictly equivalent to
	  \code{typeof(x) == "language"}.}
	\item{\code{\link{is.language}()} is also true for calls (but also
	  for \code{\link{symbol}}s and \code{\link{expression}}s where
	  \code{is.call()} is false).}
      }
    }
    \item{\code{as.call(x)}: }{Objects of mode \code{"list"} can be coerced to mode \code{"call"}.
      The first element of the list becomes the function part of the call,
      so should be a function or the name of one (as a symbol; a quoted
      string will not do).

      If you think of using \code{as.call(<string>)}, consider using
      \code{\link{str2lang}(*)} which is an efficient version of
      \code{\link{parse}(text=*)}.
      Note that \code{\link{call}()} and \code{\link{as.call}()}, when
      applicable, are much preferable to these \code{\link{parse}()} based
      approaches.
    }
  }
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

  Producing \code{\link{call}}s etc from character: \code{\link{str2lang}} and
  \code{\link{parse}}.
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
identical(quote(round(10.5)), # <- less functional, but the same
          cl) # TRUE
## such a call can also be evaluated.
eval(cl) # [1] 10

class(cl) # "call"
typeof(cl)# "language"
is.call(cl) && is.language(cl) # always TRUE for "call"s

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
