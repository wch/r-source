\name{grid.set}
\alias{grid.set}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Set the Contents of a Grid Graphical Object}
\description{
  A Grid graphical object contains a list structure;  this
  function sets the value of that list structure.
}
\usage{
grid.set(grob, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{grob}{An object of class \code{"grob"}.}
  \item{\dots}{Zero or more element-specifiers, plus a list structure.
    The list structure is required.  Each specifier
    may be a single character or numeric value.  }
}
\details{
  If there are no specifiers then the contents of the \code{grob}
  are set to be the list structure.   If there is a specifier
  and the list structure of the \code{grob} has a corresponding
  element, and that element is an object of class \code{"grob"},
  then the contents of that element are set to be the list structure.
  And so on ...

  This is ONLY for setting the list structure contents of a
  graphical object.  See \code{grid.edit} for setting the values
  of the elements of the list structure.
  
  This function should not normally be called by the user.
}
\value{
  None.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}.
}

\examples{
}
\keyword{ dplot }%-- one or more ...
