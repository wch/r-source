\name{vignetteDepends}
\alias{vignetteDepends}
\title{A function to retrieve dependency information for a vignette}
\description{
  Given a vignette name, will create a DependsList object that reports
  information about the packages the vignette depends on.
}
\usage{
vignetteDepends(vignette, recursive = TRUE, reduce = TRUE,
                local = TRUE, lib.loc = NULL)
}
\arguments{
  \item{vignette}{The name of the vignette}
  \item{recursive}{Whether or not to include indirect dependencies}
  \item{reduce}{Whether or not to collapse all sets of dependencies to a
    minimal value}
  \item{local}{Whether or not to search only locally}
  \item{lib.loc}{What libraries to search in locally}
}
\details{
  If \code{recursive} is \code{TRUE}, any package that is specified as a
  dependency will in turn have its dependencies included (and so on),
  these are known as indirect dependencies.  If recursive is
  \code{FALSE}, only the dependencies directly named by the vignette
  will be used.

  If \code{local} is \code{TRUE}, the system will only look at the
  user's local machine and not online to find dependencies.

  If \code{reduce} is \code{TRUE}, the system will collapse the fields
  in the \code{DependsList} to the minimal set of dependencies (for
  instance if the dependencies were ('foo', 'foo (>= 1.0.0)', 
  'foo (>= 1.3.0)'), the return value would be 'foo (>= 1.3.0)').
}
\value{
  An object of class \code{DependsList}
}
\author{ Jeff Gentry }
\seealso{\code{\link{pkgDepends}}}
\examples{
   ## We aren't guaranteed any vignettes in the base set of
   ## packages (yet) that I know of.
}
\keyword{utilities}
