\name{getLoadedDLLs}
\alias{getLoadedDLLs}
\alias{print.DLLInfo}
\alias{print.DLLInfoList}
\title{Get DLLs Loaded in Current Session}
\description{
  This function provides a way to get a list of all the Dynamically
  Loadable Libraries (DLLs) that are currently loaded in the current \R
  session.
}
\usage{
getLoadedDLLs()
}
\details{
  This queries the internal table that manages the DLLs.
}
\value{
  An object of class \code{"DLLInfoList"} which is a list with an
  element corresponding to each DLL that is currently loaded in the
  session.  Each element is an object of class \code{"DLLInfo"} which
  has the following entries.
  
  \item{name}{the abbreviated name.}
  \item{path}{the fully qualified name of the file which was dynamically
        loaded.}
  \item{dynamicLookup}{a logical value indicating whether R uses only
    the registration information to resolve symbols or whether it
    searches the entire symbol table of the DLL.}
  \item{handle}{a reference to the C-level data structure that
    provides access to the contents of the DLL.
    This is an object of class \code{"DLLHandle}".
  }
}
\author{Duncan Temple Lang \email{duncan@wald.ucdavis.edu}.}
\note{
  We are starting to use the \code{handle} elements in the DLL object to
  resolve symbols more directly in \R.
}
\seealso{
  \code{\link{getDLLRegisteredRoutines}},
  \code{\link{getNativeSymbolInfo}}
}
\examples{
getLoadedDLLs()
}
\keyword{interface}
