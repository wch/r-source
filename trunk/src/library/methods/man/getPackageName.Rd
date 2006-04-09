\name{getPackageName}
\alias{getPackageName}
\alias{setPackageName}
\alias{packageSlot}
\alias{packageSlot<-}
\title{The Name associated with a Given Package}
\description{
  The functions below produce the package associated with a particular
  environment or position on the search list, or of the package
  containing a particular function.  They are primarily used to support
  computations that need to differentiate objects on multiple packages.
}
\usage{
getPackageName(where)

packageSlot(object)
packageSlot(object) <- value
}
\arguments{
  \item{where}{the environment or position on the search list
    associated with the desired package.}
  \item{object}{object providing a character string name, plus the
    package in which this object is to be found.}
  \item{value}{the name of the package.}
}
\details{
  Package names are normally installed during loading of the package,
  by the \link{INSTALL} script or by the \code{\link{library}}
  function.  (Currently, the name is stored as the object
  \code{.packageName} but don't trust this for the future.)
}
\value{
  \code{packageName} return the character-string name of the package
  (without the extraneous \code{"package:"} found in the search list).

  \code{packageSlot} returns or sets the package name slot (currently
  an attribute, not a formal slot, but this will likely change).
}

\seealso{ \code{\link{search}} }

\examples{
## both the following usually return "base"
getPackageName(length(search()))

}
\keyword{ programming }% at least one, from doc/KEYWORDS
