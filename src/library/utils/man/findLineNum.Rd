% File src/library/utils/man/findLineNum.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2013 Duncan Murdoch and the R Core Team
% Distributed under GPL 2 or later

\name{findLineNum}
\alias{findLineNum}
\alias{setBreakpoint}
\title{
  Find the Location of a Line of Source Code, or Set a Breakpoint There.
}
\description{
  These functions locate objects containing particular lines of source
  code, using the information saved when the code was parsed with
  \code{keep.source = TRUE}.
}
\usage{
findLineNum(srcfile, line, nameonly = TRUE,
            envir = parent.frame(), lastenv)

setBreakpoint(srcfile, line, nameonly = TRUE,
              envir = parent.frame(), lastenv, verbose = TRUE,
              tracer, print = FALSE, clear = FALSE, ...)
}
\arguments{
  \item{srcfile}{The name of the file containing the source code.}
  \item{line}{The line number within the file.  See Details for an
    alternate way to specify this.}
  \item{nameonly}{If \code{TRUE} (the default), we require only a match
    to \code{basename(srcfile)}, not to the full path.}
  \item{envir}{Where do we start looking for function objects?}
  \item{lastenv}{Where do we stop?  See the Details.}
  \item{verbose}{Should we print information on where breakpoints were set?}
  \item{tracer}{An optional \code{tracer} function to pass to
    \code{\link{trace}}.  By default, a call to \code{\link{browser}}
    is inserted.}
  \item{print}{The \code{print} argument to pass to \code{\link{trace}}.}
  \item{clear}{If \code{TRUE}, call \code{\link{untrace}} rather than
    \code{\link{trace}}.}
  \item{\dots}{Additional arguments to pass to \code{\link{trace}}.}
}

\details{
  The \code{findLineNum} function searches through all objects in
  environment \code{envir}, its parent, grandparent, etc., all the way
  back to \code{lastenv}.

  \code{lastenv} defaults to the global environment if
  \code{envir} is not specified, and to the
  root environment \code{\link{emptyenv}()} if \code{envir} is
  specified.  (The first default tends to be quite fast, and will
  usually find all user code other than S4 methods; the second one is
  quite slow, as it will typically search all attached system
  libraries.)

  For convenience, \code{envir} may be specified indirectly:  if it is
  not an environment, it will be replaced with
  \code{environment(envir)}.

  \code{setBreakpoint} is a simple wrapper function for
  \code{\link{trace}} and \code{\link{untrace}}.  It will set or clear
  breakpoints at the locations found by \code{findLineNum}.

  The \code{srcfile} is normally a filename entered as a character
  string, but it may be a \code{"\link{srcfile}"} object, or it may
  include a suffix like \code{"filename.R#nn"}, in which case the number
  \code{nn} will be used as a default value for \code{line}.

  As described in the description of the \code{where} argument on the
  man page for \code{\link{trace}}, the \R package system uses a
  complicated scheme that may include more than one copy of a function
  in a package.  The user will typically see the public one on the
  search path, while code in the package will see a private one in the
  package NAMESPACE.  If you set \code{envir} to the environment of a
  function in the package, by default \code{findLineNum} will find both
  versions, and \code{setBreakpoint} will set the breakpoint in both.
  (This can be controlled using \code{lastenv}; e.g.  \code{envir =
  environment(foo)}, \code{lastenv = globalenv()} will find only the
  private copy, as the search is stopped before seeing the public copy.)

  S version 4 methods are also somewhat tricky to find.  They are stored
  with the generic function, which may be in the \pkg{base} or other
  package, so it is usually necessary to have \code{lastenv = emptyenv()}
  in order to find them.  In some cases transformations are done by \R
  when storing them and \code{findLineNum} may not be able to find the
  original code.  Many special cases, e.g. methods on primitive
  generics, are not yet supported.
}
\value{
  \code{fineLineNum} returns a list of objects containing location
  information.  A \code{print} method is defined for them.

  \code{setBreakpoint} has no useful return value; it is called for the
  side effect of calling \code{\link{trace}} or \code{\link{untrace}}.
}
\author{
  Duncan Murdoch
}
\seealso{
  \code{\link{trace}}
}
\examples{
\dontrun{
# Find what function was defined in the file mysource.R at line 100:
findLineNum("mysource.R#100")

# Set a breakpoint in both copies of that function, assuming one is in the
# same namespace as myfunction and the other is on the search path
setBreakpoint("mysource.R#100", envir = myfunction)
}
}
\keyword{ debugging }
