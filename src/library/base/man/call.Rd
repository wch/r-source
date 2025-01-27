% File src/library/base/man/call.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
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
      the given arguments (\code{name} must be a string which gives
      the name of a function to be called).  Note that although the call is
      unevaluated, the arguments \code{\dots} are evaluated.

      \code{call} is a primitive, so the first argument is
      taken as \code{name} and the remaining arguments as arguments for the
      constructed call: if the first argument is named the name must
      partially match \code{name}.
    }
    \item{\code{is.call} }{is used to determine whether \code{x} is a call (i.e.,
      of \code{\link{mode}} \code{"call"} or \code{"("}).  Note that
      \itemize{
	\item \code{is.call(x)} is strictly equivalent to
	  \code{typeof(x) == "language"}.
	\item \code{\link{is.language}()} is also true for calls (but also
	  for \code{\link{symbol}}s and \code{\link{expression}}s where
	  \code{is.call()} is false).
	\item When \code{is.call(cl)} is true, \code{\link{class}(cl)}
	typically returns \code{"call"}, except when \code{cl} is one of
	\code{if}, \code{for}, \code{while}, \code{(}, \code{\{},  \code{<-}, \code{=},
	which each has its own \code{class(cl)} (equal to the
	\dQuote{function} name), see the \sQuote{Special calls} example.
      }
    }
    \item{\code{as.call(x)}: }{Objects of mode \code{"list"} can be coerced to mode \code{"call"}.
      The first element of the list becomes the function part of the call,
      so should be a function or the name of one (as a symbol; a character string will not do).

      If you think of using \code{as.call(\var{string})}, consider using
      \code{\link{str2lang}(\var{string})} which is an efficient version of
      \code{\link{parse}(text=\var{string})}.
      Note that \code{call()} and \code{as.call()}, when
      applicable, are much preferable to these \code{parse()} based
      approaches.
    }
  }
  All three are \link{primitive} functions.

  \code{as.call} is generic: you can write methods to handle specific
  classes of objects, see \link{InternalMethods}.
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

  Producing \code{call}s etc from character: \code{\link{str2lang}} and
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

## Special calls (and some regular ones):
L <- as.list(E <- setNames( , c("if", "for", "while", "repeat", "function",
                                  "(", "{", "[",  "<-", "<<-", "->", "=")))
for(i in seq_along(L)) L[[i]] <- call(E[[i]]) # instead of lapply(E, call) ..
list_ <- function (...) `names<-`(list(...), vapply(sys.call()[-1L], as.character, ""))
(Tab <- noquote(sapply(list_(is.call, typeof, class, mode), \(F) sapply(L, F))))
## The 7 exceptions:
Tab[ Tab[,"class"] != "call" , c(3:4, 1:2)]

## see also the examples in the help for do.call
}
\keyword{programming}
\keyword{attribute}
