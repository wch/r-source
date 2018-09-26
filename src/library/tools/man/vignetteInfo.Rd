\name{vignetteInfo}
\alias{vignetteInfo}
\title{Basic Information about a Vignette}
\description{
  Provide basic information including package and dependency of a
  vignette from its source file.
}
\usage{
vignetteInfo(file)
}
\arguments{
  \item{file}{file name of the vignette.}
}
\value{
  a \code{\link{list}} with components, each a possibly empty
  \code{\link{character}}:
  \item{file}{the \code{\link{basename}} of the file.}
  \item{title}{the vignette title.}
  \item{depends}{the package dependencies.}
  \item{keywords}{keywords if provided.}
  \item{engine}{the vignette engine such as \code{"Sweave"},
    \code{"knitr"}, etc.}
}
\note{
  \code{vignetteInfo(file)$depends} is a substitute for the deprecated
  \code{vignetteDepends()} functionality.
}
\seealso{
  \code{\link{package_dependencies}}
}
\examples{\donttest{## This may not be installed, as it requires lattice
  gridEx <- system.file("doc", "grid.Rnw", package = "grid")
  vi <- vignetteInfo(gridEx)
  str(vi)
}}
\keyword{utilities}
