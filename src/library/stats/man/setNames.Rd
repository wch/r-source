% File src/library/stats/man/setNames.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Copyright 2005-2012 The R Foundation
% Distributed under GPL 2 or later

\name{setNames}
\alias{setNames}
\title{Set the Names in an Object}
\usage{
setNames(object = nm, nm)
}
\arguments{
 \item{object}{an object for which a \code{names} attribute will be meaningful }
 \item{nm}{a character vector of names to assign to the object}
}
\description{
  This is a convenience function that sets the names on an object and
  returns the object.  It is most useful at the end of a function
  definition where one is creating the object to be returned and would
  prefer not to store it under a name just so the names can be assigned.
}
\value{
  An object of the same sort as \code{object} with the new names assigned.
}
\author{Douglas M. Bates and Saikat DebRoy }
\seealso{\code{\link{unname}} for removing names.
}
\examples{
setNames( 1:3, c("foo", "bar", "baz") )
# this is just a short form of
tmp <- 1:3
names(tmp) <-  c("foo", "bar", "baz")
tmp

## special case of character vector, using default
setNames(nm = c("First", "2nd"))
}
\keyword{list}
