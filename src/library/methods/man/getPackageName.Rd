\name{getPackageName}
\alias{getPackageName}
\alias{functionPackageName}
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
functionPackageName(name)
}
\arguments{
  \item{where}{ The environment or position on the search list
      associated with the desired package. }
  \item{name}{The name of a function:  \code{functionPackageName}
      finds the function and returns the package name corresponding.
      Note that if there are multiple instances of the function,
      \emph{all} the corresponding names are returned.
      }
  \item{pkg}{ The name to be assigned to the package internally.
      }
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

functionPackageName("rnorm")
}
\keyword{ programming }% at least one, from doc/KEYWORDS
