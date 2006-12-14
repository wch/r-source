\name{environment}
\alias{environment}
\alias{environment<-}
\alias{.GlobalEnv}
\alias{globalenv}
\alias{emptyenv}
\alias{baseenv}
\alias{is.environment}
\alias{new.env}
\alias{parent.env}
\alias{parent.env<-}
\alias{.BaseNamespaceEnv}
\alias{environmentName}
\title{Environment Access}
\description{
  Get, set, test for and create environments.
}
\usage{
environment(fun = NULL)
environment(fun) <- value

is.environment(obj)

.GlobalEnv
globalenv()
.BaseNamespaceEnv

emptyenv()
baseenv()

new.env(hash = FALSE, parent = parent.frame())

parent.env(env)
parent.env(env) <- value

environmentName(env)
}
\arguments{
  \item{fun}{a \code{\link{function}}, a \code{\link{formula}}, or
    \code{NULL}, which is the default.}
  \item{value}{an environment to associate with the function}
  \item{obj}{an arbitrary \R object.}
  \item{hash}{a logical, if \code{TRUE} the environment will be hashed}
  \item{parent}{an environment to be used as the enclosure of the
    environment created.}
  \item{env}{an environment}
}
\value{
  If \code{fun} is a function or a formula then \code{environment(fun)}
  returns the environment associated with that function or formula.
  If \code{fun} is \code{NULL} then the current evaluation environment is
  returned.

  The replacement form sets the environment of the function or formula
  \code{fun} to the \code{value} given.

  \code{is.environment(obj)} returns \code{TRUE} iff \code{obj} is an
  \code{environment}.

  \code{new.env} returns a new (empty) environment enclosed in the
  parent's environment, by default.

  \code{parent.env} returns the parent environment of its argument.

  \code{parent.env<-} sets the enclosing environment of its first
  argument.

  \code{envrironmentName} returns a character string, that given when
  the envrionment is printed or \code{""} if it is not a named environment.
} 

\details{     
  Environments consist of a \emph{frame}, or collection of named
  objects, and a pointer to an \emph{enclosing environment}.  The most
  common example is the frame of variables local to a function call;
  its enclosure is the environment where the function was
  defined.  The enclosing environment is distinguished from the
  \emph{parent frame}:  the latter (returned by
  \code{\link{parent.frame}}) refers to the environment of the caller
  of a function.
  
  When \code{\link{get}} or \code{\link{exists}} search an environment
  with the default \code{inherits = TRUE}, they look for the variable
  in the frame, then in the enclosing frame, and so on.
    
  The global environment \code{.GlobalEnv}, more often known as the
  user's workspace, is the first item on the search path.  It can also
  be accessed by \code{globalenv()}.  On the search path, each item's
  enclosure is the next item.

  The object \code{.BaseNamespaceEnv} is the namespace environment for
  the base package.  The environment of the base package itself is
  available as \code{baseenv()}.  The ultimate enclosure of any environment
  is the empty environment \code{emptyenv()}, to which nothing may
  be assigned.
  If one follows the \code{parent.env()} chain of enclosures back far
  enough from any environment, eventually one reaches the empty
  environment.

  The replacement function \code{parent.env<-} is extremely dangerous as
  it can be used to destructively change environments in ways that
  violate assumptions made by the internal C code.  It may be removed
  in the near future.

  \code{is.environment} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\seealso{
  The \code{envir} argument of \code{\link{eval}}, \code{\link{get}},
  and \code{\link{exists}}.
  
  \code{\link{ls}} may be used to view the objects in an environment.
}
\examples{
f <- function() "top level function"

##-- all three give the same:
environment()
environment(f)
.GlobalEnv

ls(envir=environment(approxfun(1:2,1:2, method="const")))

is.environment(.GlobalEnv) # TRUE

e1 <- new.env(parent = baseenv())  # this one has enclosure package:base.
e2 <- new.env(parent = e1)
assign("a", 3, env=e1)
ls(e1)
ls(e2)
exists("a", env=e2)	# this succeeds by inheritance	   
exists("a", env=e2, inherits = FALSE)
exists("+", env=e2)	# this succeeds by inheritance
}
\keyword{data}
\keyword{programming}
