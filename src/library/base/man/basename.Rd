\name{basename}
\alias{basename}
\alias{dirname}
\title{Manipulate File Paths}
\description{
  \code{basename} removes all of the path up to the last path separator
  (if any).

  \code{dirname} returns the part of the \code{path} up to (but
  excluding) the last path separator, or \code{"."} if there is no path
  separator. 
}
\usage{
basename(path)
dirname(path)
}
\arguments{
  \item{path}{character vector, containing a single path name.}
}
\details{
  For \code{dirname} tilde expansion is done: see the description of
  \code{\link{path.expand}}.
  
  Trailing file separators are removed before dissecting the path,
  and for \code{dirname} any trailing file separators are removed
  from the result.
}
\value{
  A character vector.
}
\seealso{
  \code{\link{file.path}}, \code{\link{path.expand}}.
}
\examples{
basename(file.path("","p1","p2","p3","filename"))
dirname(file.path("","p1","p2","p3","filename"))
}
\keyword{file}
