\name{hdf5}
\title{Interface to the HDF5 Library}
\usage{
hdf5save(file, \dots)

hdf5load(file, load = TRUE)
}
\alias{hdf5save}
\alias{hdf5load}
\arguments{
  \item{file}{the name of the file in which the objects will be stored.}
  \item{\dots}{the names of the objects to be saved.}
  \item{load}{a logical value.  If \code{FALSE}, a character vector
    containing the names of the objects in the file is returned.
    If \code{TRUE} (the default), the objects are returned as the
    components of a named list.}
}
\description{
  \code{hdf5save} and \code{hd5fload} provide an experimental interface
  to version 5 of the NCSA HDF library.
}
\details{
  \code{hdf5save} writes a
  representation of \R objects to the specified file in a form which
  can be read by software which understands the HDF5 format.
  The objects can be read back from the file at a later
  date by using the function \code{hdf5load}.

  Not all \R types are supported and it probably doesn't make sense to
  put some of them into an HDF file (e.g. closures).  However, lists,
  strings, vectors, and matrices work.  Lists map to HDF groups.
  Vectors and matrices map to datasets.

  This capability is only available on machines which have the HDF5
  library, version 1.2 or higher (freely available from the reference below).
}
\author{Marcus G. Daniels \email{mgd@santafe.edu}.}
\references{\url{http://hdf.ncsa.uiuc.edu}}
\seealso{
  \code{\link{save}}, \code{\link{load}}.
}
\examples{
\dontrun{
(m <- cbind(A = 1, diag(4)))
ll <- list(a=1:10, b=letters[1:8]);
l2 <- list(C="c", l=ll); PP <- pi
hdf5save("ex1.hdf", m, PP, ll, l2)
rm(m,PP,ll,l2)	# and reload them:
hdf5load("ex1.hdf")
m        # read from "ex1.hdf"; buglet: dimnames dropped
str(ll)  # buglet: "pairlist" instead of "list"
str(l2)
}
}
\keyword{file}
