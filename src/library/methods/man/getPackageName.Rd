\name{getPackageName}
\alias{getPackageName}
\alias{setPackageName}
\title{ The name associated with a given package }
\description{
  The functions below produce the package associated with a particular
  environment or position on the search list, or of the package containing a particular
  function.  They are primarily used to support computations that need
  to differentiate objects on multiple packages.
}
\usage{
getPackageName(where = 1)
}
\arguments{
  \item{where}{ The environment or position on the search list
      associated with the desired package. }
 }
\details{
  Package names are normally installed during loading of the package,
  by the \link{INSTALL} script or by the \code{\link{library}}
  function.  (Currently, the name is stored as the object
  \code{.packageName} but don't trust this for the future.)
}
\value{
  All the functions return the character-string name of the package
  (without the extraneous \code{"package:"} found in the search list).
}

\seealso{ \code{\link{search}} }

\examples{
## both the following usually return "base"
getPackageName(length(search()))

}
\keyword{ programming }% at least one, from doc/KEYWORDS
