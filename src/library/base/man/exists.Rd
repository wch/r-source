% File src/library/base/man/exists.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{exists}
\title{Is an Object Defined?}
\alias{exists}
\alias{get0}
\description{
  Look for an \R object of the given name and possibly return it
}
\usage{
exists(x, where = -1, envir = , frame, mode = "any",
       inherits = TRUE)

get0(x, envir = pos.to.env(-1L), mode = "any", inherits = TRUE,
     ifnotfound = NULL)
}
\arguments{
  \item{x}{a variable name (given as a character string).}
  \item{where}{where to look for the object (see the details section); if
    omitted, the function will search as if the name of the object
    appeared unquoted in an expression.}
  \item{envir}{an alternative way to specify an environment to look in,
    but it is usually simpler to just use the \code{where} argument.}
  \item{frame}{a frame in the calling list.  Equivalent to giving
    \code{where} as \code{sys.frame(frame)}.}
  \item{mode}{the mode or type of object sought: see the
    \sQuote{Details} section.}
  \item{inherits}{should the enclosing frames of the environment be
    searched?}
  \item{ifnotfound}{the return value of \code{get0(x, *)} when
    \code{x} does not exist.}
}
\details{
  The \code{where} argument can specify the environment in which to look
  for the object in any of several ways: as an integer (the position in
  the \code{\link{search}} list); as the character string name of an
  element in the search list; or as an \code{\link{environment}}
  (including using \code{\link{sys.frame}} to access the currently active
  function calls).  The \code{envir} argument is an alternative way to
  specify an environment, but is primarily there for back compatibility.

  This function looks to see if the name \code{x} has a value bound to
  it in the specified environment.  If \code{inherits} is \code{TRUE} and
  a value is not found for \code{x} in the specified environment, the
  enclosing frames of the environment are searched until the name \code{x}
  is encountered.  See \code{\link{environment}} and the \sQuote{R
  Language Definition} manual for details about the structure of
  environments and their enclosures.

  \bold{Warning:}
  \code{inherits = TRUE} is the default behaviour for \R but not for S.

  If \code{mode} is specified then only objects of that type are sought.
  The \code{mode} may specify one of the collections \code{"numeric"} and
  \code{"function"} (see \code{\link{mode}}): any member of the
  collection will suffice.  (This is true even if a member of a
  collection is specified, so for example \code{mode = "special"} will
  seek any type of function.)
}
\note{
  With \code{get0()}, instead of the easy to read but somewhat
  inefficient \preformatted{
  if (exists(myVarName, envir = myEnvir)) {
    r <- get(myVarName, envir = myEnvir)
    ## ... deal with r ...
  }
}
  you now can use the more efficient (and slightly harder to read) \preformatted{
  if (!is.null(r <- get0(myVarName, envir = myEnvir))) {
    ## ... deal with r ...
  }
}
}
\value{
  \code{exists():} Logical, true if and only if an object of the correct
  name and mode is found.

  \code{get0():} The object---as from \code{\link{get}(x, *)}---
  if \code{exists(x, *)} is true, otherwise \code{ifnotfound}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{get}}.  For quite a different kind of \dQuote{existence}
  checking, namely if function arguments were specified,
  \code{\link{missing}};
  and for yet a different kind, namely if a file exists,
  \code{\link{file.exists}}.
}
\examples{
##  Define a substitute function if necessary:
if(!exists("some.fun", mode = "function"))
  some.fun <- function(x) { cat("some.fun(x)\n"); x }
search()
exists("ls", 2) # true even though ls is in pos = 3
exists("ls", 2, inherits = FALSE) # false

## These are true (in most circumstances):
identical(ls,   get0("ls"))
identical(NULL, get0(".foo.bar.")) # default ifnotfound = NULL (!)
\dontshow{
stopifnot(identical(ls, get0("ls")),
          is.null(get0(".foo.bar.")))
}
}
\keyword{data}
