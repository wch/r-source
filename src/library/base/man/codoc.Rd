\name{codoc}
\alias{codoc}
\title{Check Code/Documentation Consistency}
\usage{
codoc(dir, use.values = FALSE, use.positions = TRUE,
      ignore.generic.functions = FALSE,
      keep.tempfiles = FALSE,
      verbose = getOption("verbose"))
}
\description{
  Find inconsistencies between actual and documented usage of \R
  function objects in a package, by comparing names and optionally also
  corresponding positions and default values of the arguments of the
  functions.
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This must contain the subdirectories \file{man}
    with \R documentation sources (in Rd format) and \file{R} with \R
    code.}
  \item{use.positions}{a logical indicating whether to use the positions
    of function arguments when comparing.}
  \item{use.values}{a logical indicating whether to use function default
    values when comparing code and docs.}
  \item{ignore.generic.functions}{if \code{TRUE}, functions the body of
    which contains \code{"UseMethod"} are ignored.}
  \item{keep.tempfiles}{if \code{TRUE}, keep temporary code and docs
    files used for comparison.  This is useful for debugging.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
}
\details{
  The purpose of this function is to check whether the documented usage
  of function objects agrees with their formal arguments as defined in
  the \R code.  This is not always straightforward, in particular as the
  usage information for methods to generic functions typically employs
  the name of the generic rather than the method.

  The following algorithm is used.  The \R code of the package is
  sourced in a new environment.  Then, the usage sections of the Rd
  files are extracted and manipulated in order to give function stubs
  corresponding to the indicated usage, which are then sourced in
  another new environment.  For interpreted functions in both the code
  and docs environment, the formals are compared according to the values
  of the arguments \code{use.positions} and \code{use.values}.

  Currently, synopsis sections are used, but multiple usage examples
  (such as in \code{\link{abline}}) are not combined when building the
  stubs.
}
\value{
  A list the names of which are the names of the functions where an
  inconsistency was found.  The elements of the list are lists of length
  2 with elements \code{code} and \code{docs}, giving the corresponding
  arguments obtained from the function's code and documented usage.
}
\seealso{
  \code{\link{undoc}}
}
\keyword{documentation}
