% File src/library/utils/man/combn.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2012-3 R Core Team
% Distributed under GPL 2 or later

\name{globalVariables}
\alias{globalVariables}
\title{
  Declare Variables to be Treated as Global in Checking a Package
}
\description{
  The names supplied are of functions or other objects that should be
  regarded as defined globally when the \code{check} tool is applied to
  this package.  The call to \code{globalVariables} will be included in
  the package's source.  Repeated calls in the same package accumulate
  the names of the global variables.

  Typical examples are the fields and methods in reference classes,
  which appear to be global objects to \code{codetools}.
  (This case is handled automatically by \code{\link{setRefClass}()} and
  friends, using the supplied field and method names.)
}
\usage{
globalVariables(names, package, add = TRUE)
}
\arguments{
  \item{names}{
    The character vector of object names.  If omitted, the current list of
    global variables declared in the package will be returned, unchanged.
  }
  \item{package}{
    The relevant package, usually the character string name of the package
    but optionally its corresponding namespace environment.

    When the call to \code{globalVariables} comes in the package's source
    file, the argument is normally omitted, as in the example below.
  }
  \item{add}{
    Should the contents of \code{names} be added to the current global
    variables or replace it?
  }
}
\details{
  The list of declared global variables is stored in a metadata object
  in the package's namespace, assuming the \code{globalVariables} call(s)
  occur as top-level calls in the package's source code.

  The check command, as implemented in package \code{utils}, queries the
  global variables list before checking the \R{} source code in the
  package for possible problems.

  This was introduced in \R 2.15.1 so should be used conditionally: see
  the example.
}
\value{
  The current list of declared global variables, possibly modified by
  this call.
}
\author{
  John Chambers
}
\note{
  The global variables list really belongs to a restricted scope (a
  function or a group of method definitions, for example) rather than
  the package as a whole.  However, implementing finer control would
  require changes in \code{check} and/or in \code{codetools}, so in this
  version the information is stored at the package level.
}

\examples{
\dontrun{
## assume your package has some code that assigns ".obj1" and ".obj2"
## but not in a way that codetools can find.
## In the same source file (to remind you that you did it) add:
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".obj1", "obj2"))
}
}
\keyword{ packages }

