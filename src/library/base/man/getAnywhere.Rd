\name{getAnywhere}
\alias{getAnywhere}
\alias{[.getAnywhere}
\alias{print.getAnywhere}
\title{Retrieve an R Object, Including from a Namespace}
\description{
  This functions locates all objects with name matching its argument,
  whether visible on the search path, registered as an S3 method or in a
  namespace but not exported.
}
\usage{
getAnywhere(x)
}
\arguments{
  \item{x}{a character string or name.}
}
%\details{
%}
\value{
  An object of class \code{"getAnywhere"}.  This is a list with components
  \item{name}{the name searched for.}
  \item{funs}{a list of objects found}
  \item{where}{a character vector explaining where the object(s) were found}
  \item{visible}{logical: is the object visible}
  \item{dups}{logical: is the object identical to one earlier in the
    list.}

  Normally the structure will be hidden by the \code{print} method.
  There is a \code{[} method to extract one or more of the objects found.
}
\seealso{
  \code{\link{get}},  \code{\link{getFromNamespace}}
}
\examples{
getAnywhere("format.dist")
getAnywhere("simpleLoess") # not exported from modreg
}
\keyword{data}
