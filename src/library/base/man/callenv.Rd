% File src/library/base/man/callenv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{callenv}
\alias{.addLocalToCallenv}
\alias{.getEnvVarsInfo}
\alias{.strictCallVars}
\title{Call Environment Utilities}
\description{
  Experimental utilities for call environments.
}
\usage{
.getEnvVarsInfo(envir)
.addLocalToCallenv(name, value, envir)
.strictCallVars(new)
}
\arguments{
  \item{name}{a variable name, given as a character string.}
  \item{value}{an optional value to be assigned to \code{name}.}
  \item{envir}{the \code{\link{environment}} to use.}
  \item{new}{logical; new value.}
}
\details{
  The environment created when a function closure is called contains as
  an attribute information about possible local variables created by the
  function, and about global functions and variables that might be used
  by the function. These are determined by static analysis of the
  function's code. This information is used to reduce differences
  between byte compiled and interpreted code. There are two options:

  \itemize{
    \item A strict approach only allows new bindings to be created in a
    call environment for variables that have been identified as possible
    local variables in the code.

    \item A less strict approach only rules out creating bindings for
    variables that have been identified as possible global variables.
  }  

  The strict setting can be selected by starting R with the environment
  variable code{R_STRICT_VAR_CHECK} set. The setting can be changed
  within a session using \code{.strictCallVars}. This function returns
  the previous value.
  
  The function \code{.getEnvVarsInfo} accesses the variable information
  and returns a named list of character vectors.  The elements are the
  names of global variables (\code{gvars}), global functions
  (\code{gfuns}) and local variables (\code{locs}) of the closure.
    
  \code{.addLocalToCallenv} provides a means to add a variable binding
  that would otherwise not be allowed. This function adds \code{name} to
  the known local variables for the environment \code{envir}, which
  should be an environment for a call created when applying a
  closure. If \code{value} is not missing, then it is assigned as the
  value of \code{name} in \code{envir}. \bold{This needs to be used with
  caution.  If the code of the function references the variable as a
  global, then the compiler may have optimized the reference to the
  global.}
}

\examples{
## capture a call environment:
env.F <- (function(x) environment())(1)

## show the variable info for the called closure:
.getEnvVarsInfo(env.F)

## with strict checking, assigning to a variable not in the locals list
## signals an error:
old.strict <- .strictCallVars(TRUE)
try(assign("y", 1, env.F))

## after adding the variable to the locls list the assignment succeeds:
.addLocalToCallenv("y", envir = env.F)
assign("y", 1, env.F)
invisible(.strictCallVars(old.strict))
}
\keyword{data}
