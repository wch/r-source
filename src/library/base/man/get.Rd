% File src/library/base/man/get.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{get}
\alias{get}
\alias{mget}
\title{Return the Value of a Named Object}
\description{
  Search by name for an object (\code{get}) or zero or more objects
  (\code{mget}).
}
\usage{
get(x, pos = -1, envir = as.environment(pos), mode = "any",
    inherits = TRUE)

mget(x, envir = as.environment(-1), mode = "any", ifnotfound,
     inherits = FALSE)
}
\arguments{
  \item{x}{For \code{get}, an object name (given as a character
    string).\cr
    For \code{mget}, a character vector of object names.
  }
  \item{pos, envir}{where to look for the object (see \sQuote{Details}); if
    omitted search as if the name of the object appeared unquoted in an
    expression.}
  \item{mode}{the mode or type of object sought: see the
    \sQuote{Details} section.}
  \item{inherits}{should the enclosing frames of the environment be
    searched?}
  \item{ifnotfound}{A \code{\link{list}} of values to be used if the item is
    not found: it will be coerced to a list if necessary.}
}
\details{
  The \code{pos} argument can specify the environment in which to look
  for the object in any of several ways: as a positive integer (the
  position in the \code{\link{search}} list); as the character string
  name of an element in the search list; or as an
  \code{\link{environment}} (including using \code{\link{sys.frame}}
  to access the currently active function calls).  The default of
  \code{-1} indicates the current environment of the call to
  \code{get}. The \code{envir} argument is an alternative way to
  specify an environment.

  These functions look to see if each of the name(s) \code{x} have a
  value bound to it in the specified environment.  If \code{inherits} is
  \code{TRUE} and a value is not found for \code{x} in the specified
  environment, the enclosing frames of the environment are searched
  until the name \code{x} is encountered.  See \code{\link{environment}}
  and the \sQuote{R Language Definition} manual for details about the
  structure of environments and their enclosures.

  If \code{mode} is specified then only objects of that type are sought.
  \code{mode} here is a mixture of the meanings of \code{\link{typeof}}
  and \code{\link{mode}}: \code{"function"} covers primitive functions
  and operators, \code{"numeric"}, \code{"integer"} and \code{"double"}
  all refer to any numeric type, \code{"symbol"} and \code{"name"} are
  equivalent \emph{but} \code{"language"} must be used (and not
  \code{"call"} or \code{"("}).

  For \code{mget}, the values of \code{mode} and \code{ifnotfound} can
  be either the same length as \code{x} or of length 1.  The argument
  \code{ifnotfound} must be a list containing either the value to use if
  the requested item is not found or a function of one argument which
  will be called if the item is not found, with argument the name of the
  item being requested.
}
\value{
  For \code{get}, the object found.  If no object is found an error results.

  For \code{mget}, a named list of objects (found or specified \emph{via}
  \code{ifnotfound}).
}
\note{
  The reverse of \code{a <- get(nam)} is \code{\link{assign}(nam, a)}.
  
  \code{inherits = TRUE} is the default for \code{get} in \R
  but not for S where it had a different meaning.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{exists}}, \code{\link{assign}}.
}
\examples{
get("\%o\%")

## test mget
e1 <- new.env()
mget(letters, e1, ifnotfound = as.list(LETTERS))
}
\keyword{data}
