\name{grid.points}
\alias{grid.points}
\title{Draw Data Symbols in a Grid Viewport}
\description{
  This function draws data symbols.
}
\usage{
grid.points(x = runif(10),
            y = runif(10)),
            pch = 1, size = unit(1, "char"),
            default.units = "native", 
            gp=gpar(), draw = TRUE, vp = NULL)
}
\arguments{
  \item{x}{A numeric vector or unit object specifying x-values.}
  \item{y}{A numeric vector or unit object specifying y-values.}
  \item{pch}{A numeric or character vector
    indicating what sort of plotting symbol to use.}
  \item{size}{A unit object specifying the size of the plotting symbols.}
  \item{default.units}{A string indicating the default units to use
    if \code{x} or \code{y} are only given as numeric vectors.}  
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{draw}{A logical value indicating whether graphics output
    should be produced.}
  \item{vp}{A Grid viewport object (or NULL).}
}
\value{
  An object of class \code{"grob"}.
}
\details{
  The \code{"grob"} object contains an object of class \code{"points"}.
}
\author{Paul Murrell}
\seealso{
  \link{Grid},
  \code{\link{viewport}}
}
\keyword{dplot}
