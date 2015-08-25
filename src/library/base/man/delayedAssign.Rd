% File src/library/base/man/delayedAssign.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{delayedAssign}
\alias{delayedAssign}
\alias{promise}
\alias{promises}
\title{Delay Evaluation}
\description{
  \code{delayedAssign} creates a \emph{promise} to evaluate the given
  expression if its value is requested.  This provides direct access
  to the \emph{lazy evaluation} mechanism used by \R for the evaluation
  of (interpreted) functions.
}
\usage{
delayedAssign(x, value, eval.env = parent.frame(1),
              assign.env = parent.frame(1))
}
\arguments{
  \item{x}{a variable name (given as a quoted string in the function call)}
  \item{value}{an expression to be assigned to \code{x}}
  \item{eval.env}{an environment in which to evaluate \code{value}}
  \item{assign.env}{an environment in which to assign \code{x}}
}
\value{
  This function is invoked for its side effect, which is assigning
  a promise to evaluate \code{value} to the variable \code{x}.
}
\details{
  Both \code{eval.env} and \code{assign.env} default to the currently active
  environment.

  The expression assigned to a promise by \code{delayedAssign} will
  not be evaluated until it is eventually \sQuote{forced}.  This happens when
  the variable is first accessed.

  When the promise is eventually forced, it is evaluated within the
  environment specified by \code{eval.env} (whose contents may have changed in
  the meantime).  After that, the value is fixed and the expression will
  not be evaluated again.
}
\seealso{
  \code{\link{substitute}}, to see the expression associated with a
  promise, if \code{assign.env} is not the \code{\link{.GlobalEnv}}.
}
\examples{
msg <- "old"
delayedAssign("x", msg)
substitute(x) # shows only 'x', as it is in the global env.
msg <- "new!"
x # new!

delayedAssign("x", {
    for(i in 1:3)
        cat("yippee!\n")
    10
})

x^2 #- yippee
x^2 #- simple number

ne <- new.env()
delayedAssign("x", pi + 2, assign.env = ne)
## See the promise {without "forcing" (i.e. evaluating) it}:
substitute(x, ne) #  'pi + 2'
\dontshow{stopifnot(identical(substitute(x,ne), quote(pi + 2)))}

### Promises in an environment [for advanced users]:  ---------------------

e <- (function(x, y = 1, z) environment())(cos, "y", {cat(" HO!\n"); pi+2})
## How can we look at all promises in an env (w/o forcing them)?
gete <- function(e_)
   lapply(lapply(ls(e_), as.name),
          function(n) eval(substitute(substitute(X, e_), list(X=n))))

(exps <- gete(e))
sapply(exps, typeof)

(le <- as.list(e)) # evaluates ("force"s) the promises
stopifnot(identical(unname(le), lapply(exps, eval))) # and another "Ho!"
}
\keyword{programming}
\keyword{data}
