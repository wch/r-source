\name{grid.get}
\alias{grid.get}
\title{Get the Contents of a Grid Graphical Object}
\description{
  A Grid graphical object contains a list structure;  this
  function returns that list structure.
}
\usage{
grid.get(grob, ...)
}
\arguments{
  \item{grob}{An object of class \code{"grob"}.}
  \item{\dots}{Zero or more element-specifiers.  Each specifier
    may be a single character or numeric value.}
}
\details{
  If there are no specifiers then the value returned is just the
  list structure of the \code{grob}.  If there is a specifier
  and the list structure of the \code{grob} has a corresponding
  element, and that element is an object of class \code{"grob"},
  then the return value is the list structure within that element.
  And so on ...

  Typically, users will not need to call this function, even
  when writing their own graphical objects.  Most graphical object
  methods will work only with the list structure.
}
\value{
  A list structure.
}
\author{Paul Murrell}
\seealso{
  \code{\link{grid.grob}}.
}
\examples{
xa <- grid.xaxis(draw=FALSE)
grid.get(xa)
grid.get(xa, "ticks")

temp <- grid.collection(axis=grid.xaxis(draw=FALSE), draw=FALSE)
grid.get(temp, "children", "axis", "ticks")
}
\keyword{dplot}
