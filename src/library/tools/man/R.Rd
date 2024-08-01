% File src/library/tools/man/R.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2024 R Core Team
% Distributed under GPL 2 or later

\name{R}
\alias{R}
\title{Call Function in Inferior R Process}
\description{
  Call a function with given arguments in a new (\dQuote{inferior}) R
  process.
}
\usage{
R(fun, args = list(), opts = "--no-save --no-restore", 
  env = character(), arch = "", drop = TRUE, timeout = 0)
}
\arguments{
  \item{fun}{a function to call in the inferior R process.}
  \item{args}{a list of arguments to the function call.}
  \item{opts}{a character vector of command line options to use when
    starting the inferior R process.}
  \item{env}{a character vector of name=value strings to set environment
    variables to use when starting the inferior R process.}
  \item{arch}{a character string giving a sub-architecture to use for
    the inferior R process.}
  \item{drop}{a logical controlling what gets returned in case of
    \dQuote{success} (when there was no error running the inferior R
    process and the value of the function call is not a condition
    object).  By default, in this case only that value is returned.
    If \code{drop = FALSE}, a list with that value and status, stdout
    and stderr of running the inferior R process.}
  \item{timeout}{a timeout in seconds for the inferior R process,
    ignored if 0.}
}
\details{
  The given function and argument list are serialized into a
  \I{tempfile}.
  Then, an inferior R process is run via (a wrapper for)
  \code{\link{system2}()} as controlled by arguments \code{opts},
  \code{env}, \code{arch} and \code{timeout}, the input of which loads
  the \I{tempfile} and calls the function with the argument list.
  It is ensured that an appropriate CRAN mirror is set.
}
\value{
  In case of \dQuote{success} (see above), the value of the function
  call if \code{drop = TRUE} (default), or a named list with the value
  and the status, stdout and stderr of running the inferior R process.

  Otherwise, currently an error condition object inheriting from
  \code{"inferiorCallError"} with the result (status, stdout and stderr)
  of running the inferior R process, and in case this was successfully
  run but the value of the function call is a condition object, that
  object in the \code{value} element.

  What gets returned in the non-success case may change in the future,
  perhaps using two different classes for the two cases of
  \dQuote{failure} (i.e., failure in running the inferior R process or
  calling the function).
}
\examples{
## Compute cos(0) in an inferior R process.
## By default, only return the value of the function call.
R(cos, list(0))
## If 'drop = FALSE', we also get status, stdout and stderr.
\dontdiff{
R(cos, list(0), drop = FALSE)
}    

## A call giving an error:
(e <- tryCatch(R(stop, list("FOOBAR")), error = identity))
## The inferior R process ran successfully:
e$status
## The function call gave an error:
e$value
}
