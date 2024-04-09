% File src/library/base/man/Tailcall.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{Tailcall}
\alias{Tailcall}
\alias{Exec}
\title{\code{Tailcall} and \code{Exec}}
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
  
  \code{Tailcall} evaluates a call to \code{FUN} with arguments \dots in
  the current environment, and \code{Exec} evaluates the call
  \code{expr} in environment \code{envir}. If a \code{Tailcall} or
  \code{Exec} expression appears in tail position in an \R function, and
  if there are no \code{on.exit} expressions set, then the evaluation
  context of the new calls replaces the currently executing call context
  with a new one. If the requirements for context re-use are not met,
  then evaluation proceeds in the standard way adding another context to
  the stack.

  Using \code{Tailcall} it is possible to define tail-recursive
  functions that do not grow the evaluation stack.  \code{Exec} can be
  used to simplify the call stack for functions that create and then
  evaluate an expression.

  Because of lazy evaluation of arguments in \R it may be necessary to
  force evaluation of some arguments to avoid accumulating deferred
  evaluations.

  This \emph{tail call optimization} has the advantage of not growing
  the call stack and permitting arbitrarily deep tail recursions. It
  does also mean that stack traces produced by \code{\link{traceback}}
  or \code{\link{sys.calls}} will only show the call specified by
  \code{Tailcall} or \code{Exec}, not the previous call whose stack
  entry has been replaced.
}
\note{\code{Tailcall} and \code{Exec} are experimental and may be
  changed or dropped in future released versions of \R.
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
## the call stack does not contain the call to docall:
docall(function() sys.calls(), list()) |> 
    Find(function(x) identical(x[[1]], quote(docall)), x = _)
## contrast to do.call:
do.call(function(x) sys.calls(), list()) |> 
    Find(function(x) identical(x[[1]], quote(do.call)), x = _)
}
\keyword{programming}

