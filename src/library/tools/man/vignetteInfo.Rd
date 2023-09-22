% File src/library/tools/man/vignetteInfo.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2018-2022 R Core Team
% Distributed under GPL 2 or later

\name{vignetteInfo}
\alias{vignetteInfo}
\title{Basic Information about a Vignette}
\description{
  Extract metadata from a vignette source file.
}
\usage{
vignetteInfo(file)
}
\arguments{
  \item{file}{file name of the vignette.}
}
\value{
  A \code{\link{list}} with the following
  \code{\link{character}} components:
  
  \item{file}{the \code{\link{basename}} of the file.}
  \item{title}{the vignette title from
    \samp{\VignetteIndexEntry}, possibly an empty string.}
  \item{depends}{a vector of package dependencies from
    \samp{\VignetteDepends}, possibly of length 0.}
  \item{keywords}{a vector of keywords from
    \samp{\VignetteKeyword}, possibly of length 0.}
  \item{engine}{the \code{\link{vignetteEngine}}, such as
    \code{"utils::Sweave"} or \code{"knitr::knitr"}.}
}
\seealso{
  \code{\link{package_dependencies}} for recursive dependencies.
}
\examples{
  gridEx <- system.file("doc", "grid.Rnw", package = "grid")
  vi <- vignetteInfo(gridEx)
  str(vi)
}
\keyword{utilities}
