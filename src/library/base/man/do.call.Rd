% File src/library/base/man/do.call.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{do.call}
\title{Execute a Function Call}
\usage{
do.call(what, args, quote = FALSE, envir = parent.frame())
}
\alias{do.call}
\arguments{
  \item{what}{either a function or a non-empty character string naming the
    function to be called.}
  \item{args}{a \emph{list} of arguments to the function call.  The
    \code{names} attribute of \code{args} gives the argument names.}
  \item{quote}{a logical value indicating whether to quote the
    arguments.}
  \item{envir}{an environment within which to evaluate the call.  This
    will be most useful if \code{what} is a character string and
    the arguments are symbols or quoted expressions.}
}
\description{
  \code{do.call} constructs and executes a function call from a name or
  a function and a list of arguments to be passed to it.
}
\details{
  If \code{quote} is \code{FALSE}, the default, then the arguments are
  evaluated (in the calling environment, not in \code{envir}).  If
  \code{quote} is \code{TRUE} then each argument is quoted (see
  \code{\link{quote}}) so that the effect of argument evaluation is to
  remove the quotes -- leaving the original arguments unevaluated when the
  call is constructed.

  The behavior of some functions, such as \code{\link{substitute}},
  will not be the same for functions evaluated using \code{do.call} as
  if they were evaluated from the interpreter.  The precise semantics
  are currently undefined and subject to change.
}
\value{
  The result of the (evaluated) function call.
}
\section{Warning}{
  This should not be used to attempt to evade restrictions on the use of
  \code{.Internal} and other non-API calls.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{call}} which creates an unevaluated call.
}
\examples{
do.call("complex", list(imag = 1:3))

## if we already have a list (e.g., a data frame)
## we need c() to add further arguments
tmp <- expand.grid(letters[1:2], 1:3, c("+", "-"))
do.call("paste", c(tmp, sep = ""))

do.call(paste, list(as.name("A"), as.name("B")), quote = TRUE)

## examples of where objects will be found.
A <- 2
f <- function(x) print(x^2)
env <- new.env()
assign("A", 10, envir = env)
assign("f", f, envir = env)
f <- function(x) print(x)
f(A)                                      # 2
do.call("f", list(A))                     # 2
do.call("f", list(A), envir = env)        # 4
do.call(f, list(A), envir = env)          # 2
do.call("f", list(quote(A)), envir = env) # 100
do.call(f, list(quote(A)), envir = env)   # 10
do.call("f", list(as.name("A")), envir = env) # 100

eval(call("f", A))                      # 2
eval(call("f", quote(A)))               # 2
eval(call("f", A), envir = env)         # 4
eval(call("f", quote(A)), envir = env)  # 100
}
\keyword{programming}
