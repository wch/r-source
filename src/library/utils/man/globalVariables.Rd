% File src/library/utils/man/globalVariables.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2012-2017 R Core Team
% Distributed under GPL 2 or later

\name{globalVariables}
\alias{globalVariables}
\alias{suppressForeignCheck}
\title{
  Declarations Used in Checking a Package
}
\description{
  For \code{globalVariables}, the names supplied are of functions or
  other objects that should be regarded as defined globally when the
  \code{check} tool is applied to this package.  The call to
  \code{globalVariables} will be included in the package's source.
  Repeated calls in the same package accumulate the names of the
  global variables.

  Typical examples are the fields and methods in reference classes,
  which appear to be global objects to \code{codetools}.
  (This case is handled automatically by \code{\link{setRefClass}()} and
  friends, using the supplied field and method names.)
  
  For \code{suppressForeignCheck}, the names supplied are of variables
  used as \code{.NAME} in foreign function calls which should not be
  checked by \code{\link{checkFF}(registration = TRUE)}.  Without this
  declaration, expressions other than simple character strings are
  assumed to evaluate to registered native symbol objects.  The type of
  call (\code{.Call}, \code{.External}, etc.) and argument counts will
  be checked.  With this declaration, checks on those names will usually
  be suppressed.  (If the code uses an expression that should only be
  evaluated at runtime, the message can be suppressed by wrapping it in
  a \code{\link{dontCheck}} function call, or by saving it to a local
  variable, and suppressing messages about that variable. See the
  example below.)
}
\usage{
globalVariables(names, package, add = TRUE)
suppressForeignCheck(names, package, add = TRUE)
}
\arguments{
  \item{names}{
    The character vector of object names.  If omitted, the current list of
    global variables declared in the package will be returned, unchanged.
  }
  \item{package}{
    The relevant package, usually the character string name of the package
    but optionally its corresponding namespace environment.

    When the call to \code{globalVariables} or
    \code{suppressForeignCheck} comes in the package's source file,
    the argument is normally omitted, as in the example below.
  }
  \item{add}{
    Should the contents of \code{names} be added to the current global
    variables or replace it?
  }
}
\details{
  The lists of declared global variables and native symbol objects are
  stored in a metadata object in the package's namespace, assuming the
  \code{globalVariables} or \code{suppressForeignCheck} call(s) occur
  as top-level calls in the package's source code.

  The check command, as implemented in package \code{tools}, queries
  the list before checking the \R{} source code in the package for
  possible problems.

  \code{globalVariables} was introduced in \R 2.15.1 and
  \code{suppressForeignCheck} was introduced in \R 3.1.0 so both
  should be used conditionally: see the example.  
}
\value{
  \code{globalVariables} returns the current list of declared global
  variables, possibly modified by this call.
  
  \code{suppressForeignCheck} returns the current list of native
  symbol objects which are not to be checked.  
}
\author{
  John Chambers and Duncan Murdoch
}
\note{
  The global variables list really belongs to a restricted scope (a
  function or a group of method definitions, for example) rather than
  the package as a whole.  However, implementing finer control would
  require changes in \code{check} and/or in \code{codetools}, so in this
  version the information is stored at the package level.
}
\seealso{
  \code{dontCheck}.
}
\examples{
\dontrun{
## assume your package has some code that assigns ".obj1" and ".obj2"
## but not in a way that codetools can find.
## In the same source file (to remind you that you did it) add:
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".obj1", "obj2"))

## To suppress messages about a run-time calculated native symbol, 
## save it to a local variable.

## At top level, put this:
if(getRversion() >= "3.1.0") utils::suppressForeignCheck("localvariable")

## Within your function, do the call like this:
localvariable <- if (condition) entry1 else entry2
.Call(localvariable, 1, 2, 3)

## HOWEVER, it is much better practice to write code
## that can be checked thoroughly, e.g.
if(condition) .Call(entry1, 1, 2, 3) else .Call(entry2, 1, 2, 3)
}
}
\keyword{ packages }

