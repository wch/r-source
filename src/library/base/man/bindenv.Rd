% File src/library/base/man/bindenv.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{bindenv}
\title{Binding and Environment Locking, Active Bindings}
\usage{
lockEnvironment(env, bindings = FALSE)
environmentIsLocked(env)
lockBinding(sym, env)
unlockBinding(sym, env)
bindingIsLocked(sym, env)

makeActiveBinding(sym, fun, env)
bindingIsActive(sym, env)
}
\alias{bindenv}
\alias{lockEnvironment}
\alias{environmentIsLocked}
\alias{lockBinding}
\alias{unlockBinding}
\alias{makeActiveBinding}
\alias{bindingIsLocked}
\alias{bindingIsActive}
\arguments{
  \item{env}{an environment.}
  \item{bindings}{logical specifying whether bindings should be locked.}
  \item{sym}{a name object or character string.}
  \item{fun}{a function taking zero or one arguments.}
}
\description{
  These functions represent an experimental interface for adjustments
  to environments and bindings within environments.  They allow for
  locking environments as well as individual bindings, and for linking
  a variable to a function.
}
\details{
  The function \code{lockEnvironment} locks its environment argument,
  which must be a normal environment (not base).  (Locking the base
  environment and namespace may be supported later.)  Locking the
  environment prevents adding or removing variable bindings from the
  environment.  Changing the value of a variable is still possible unless
  the binding has been locked.  The namespace environments of packages
  with namespaces are locked when loaded.

  \code{lockBinding} locks individual bindings in the specified
  environment.  The value of a locked binding cannot be changed.  Locked
  bindings may be removed from an environment unless the environment is
  locked.

  \code{makeActiveBinding} installs \code{fun} in environment \code{env}
  so that getting the value of \code{sym} calls \code{fun} with no
  arguments, and assigning to \code{sym} calls \code{fun} with one
  argument, the value to be assigned.  This allows the implementation of
  things like C variables linked to \R variables and variables linked to
  databases, and is used to implement \code{\link{setRefClass}}.  It may
  also be useful for making thread-safe versions of some system globals.
}
\value{
  The \code{bindingIsLocked} and \code{environmentIsLocked} return a
  length-one logical vector.  The remaining functions return
  \code{NULL}, invisibly.
}
\examples{
# locking environments
e <- new.env()
assign("x", 1, envir = e)
get("x", envir = e)
lockEnvironment(e)
get("x", envir = e)
assign("x", 2, envir = e)
try(assign("y", 2, envir = e)) # error

# locking bindings
e <- new.env()
assign("x", 1, envir = e)
get("x", envir = e)
lockBinding("x", e)
try(assign("x", 2, envir = e)) # error
unlockBinding("x", e)
assign("x", 2, envir = e)
get("x", envir = e)

# active bindings
f <- local( {
    x <- 1
    function(v) {
       if (missing(v))
           cat("get\n")
       else {
           cat("set\n")
           x <<- v
       }
       x
    }
})
makeActiveBinding("fred", f, .GlobalEnv)
bindingIsActive("fred", .GlobalEnv)
fred
fred <- 2
fred
}
\keyword{utilities}
\author{Luke Tierney}
