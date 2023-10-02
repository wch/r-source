% File src/library/base/man/Tailcall.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{Tailcall}
\alias{Tailcall}
\alias{Exec}
\title{Tailcall and Exec}
\description{
  \code{Tailcall} and \code{Exec} allow writing more
  stack-space-efficient recursive functions in \R.
 }
\usage{
Tailcall(FUN, \dots)
Exec(expr, envir)
}
\arguments{
  \item{FUN}{a function or a non-empty character string naming the
    function to be called.}
  \item{\dots}{all the arguments to be passed.}
  \item{expr}{a call expression.}
  \item{envir}{environment for evaluating \code{expr}; default is the
    environment from which \code{Exec} is called.
  }
}
\details{
  \code{Tailcall} and \code{Exec} can only be used inside an \R function,
  not at top level. They replace the currently executing call context
  with a new one for the call to \code{FUN} or the call specified in
  \code{expr}.

  Using \code{Tailcall} it is possible to define
  tail-recursive functions that do not grow the evaluation stack.
  Because of lazy evaluation it may be necessary to \code{force} some
  arguments to avoid accumulating deferred evaluations.
  
  \code{Exec} can be used to simplify the call stack for functions that
  create and then evaluate an expression.
}
\note{\code{Tailcall} and \code{Exec} are experimental and may not be
  included in a released version of \R.
}
\seealso{
  \code{\link{Recall}} and \code{\link{force}}.
}
\examples{
## tail-recursive log10-factorial
lfact <- function(n) {
    lfact_iter <- function(val, n) {
        if (n <= 0)
            val
        else {
            val <- val + log10(n) # forces val
            Tailcall(lfact_iter, val, n - 1)
        }
    }
    lfact_iter(0, n)
}
10 ^ lfact(3)
lfact(100000)

## simplified variant of do.call using Exec:
docall <- function (what, args, quote = FALSE) {
    if (!is.list(args)) 
        stop("second argument must be a list")
    if (quote) 
        args <- lapply(args, enquote)
    Exec(as.call(c(list(substitute(what)), args)), parent.frame())
}
## the call stack doe not contain the call to docall:
docall(function(x) sys.calls(), list(1)) |> 
    Find(function(x) identical(x[[1]], quote(docall)), x = _)
## contrast to do.call:
do.call(function(x) sys.calls(), list(1)) |> 
    Find(function(x) identical(x[[1]], quote(do.call)), x = _)
}
\keyword{programming}

