% File src/library/base/man/ns-dblcolon.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{ns-dblcolon}
\title{Double Colon and Triple Colon Operators}
\alias{::}
\alias{:::}
\description{
  Accessing exported and internal variables in a name space, or
  variables in an attached package.
}
\usage{
pkg::name
pkg:::name
}
\arguments{
  \item{pkg}{package name: symbol or literal character string.}
  \item{name}{variable name: symbol or literal character string.}
}
\details{
  For a package with a name space, \code{pkg::name} returns the value of
  the exported variable \code{name} in name space \code{pkg}, whereas
  \code{pkg:::name} returns the value of the internal variable
  \code{name}. The name space will be loaded if it was not loaded
  before the call, but the package will not be attached to the
  search path.

  If the package \code{pkg} does not have a name space but is on the
  search path then \code{pkg::name} returns the value of \code{name} in
  the package environment.  Thus \code{pkg::name} has the same effect
  for attached packages whether or not they have a name space.

  Specifying a variable that does not exist is an error, as is
  specifying a package that does not exist or does not have a name space
  and is not on the search path.

  Note that it is typically a design mistake to use \code{:::}
  in your code since the corresponding object has probably been kept
  internal for a good reason.  Consider contacting the package
  maintainer if you feel the need to access the object for anything but
  mere inspection.
}
\seealso{
  \code{\link{get}} to access an object masked by another of the same name.
}
\examples{
base::log
base::"+"

## Beware --  use ':::' at your own risk! (see "Details")
stats:::coef.default
}
\keyword{programming}
