% File src/library/grDevices/man/densCols.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later


\name{densCols}
\alias{densCols}
\alias{blues9}
\title{Colors for Smooth Density Plots}
\description{
  \code{densCols} produces a vector containing colors which encode the
  local densities at each point in a scatterplot.
}
\usage{
densCols(x, y = NULL, nbin = 128, bandwidth,
         colramp = colorRampPalette(blues9[-(1:3)]))
blues9
}
\arguments{
  \item{x, y}{the \code{x} and \code{y} arguments provide the x and y
    coordinates of the points.  Any reasonable way of defining the
    coordinates is acceptable.  See the function \code{\link{xy.coords}}
    for details.  If supplied separately, they must be of the same length.}
  \item{nbin}{numeric vector of length one (for both directions) or two
    (for x and y separately) specifying the number of equally spaced
    grid points for the density estimation; directly used as
    \code{gridsize} in \code{\link[KernSmooth]{bkde2D}()}.}
  \item{bandwidth}{numeric vector (length 1 or 2) of smoothing bandwidth(s).
    If missing, a more or less useful default is used. \code{bandwidth}
    is subsequently passed to function \code{\link[KernSmooth]{bkde2D}}.}
  \item{colramp}{function accepting an integer \code{n} as an argument and
    returning \code{n} colors.}
}

\details{
  \code{densCols} computes and returns the set of colors that will be
  used in plotting, calling \code{\link[KernSmooth]{bkde2D}(*,
  bandwidth, gridsize = nbin, ..)} from package \CRANpkg{KernSmooth}.

  \code{blues9} is a set of 9 color shades of blue used as the default in
  plotting.
}

\value{
  \code{densCols} returns a vector of length \code{nrow(x)} that
  contains colors to be used in a subsequent scatterplot.  Each color
  represents the local density around the corresponding point.
}
\seealso{
  \code{\link[KernSmooth]{bkde2D}} from package \CRANpkg{KernSmooth};
  further, \code{\link{smoothScatter}()} (package \pkg{graphics}) which
  builds on the same computations as \code{densCols}.
}
\author{Florian Hahne at FHCRC, originally}
\examples{\donttest{
x1  <- matrix(rnorm(1e3), ncol = 2)
x2  <- matrix(rnorm(1e3, mean = 3, sd = 1.5), ncol = 2)
x   <- rbind(x1, x2)

dcols <- densCols(x)
graphics::plot(x, col = dcols, pch = 20, main = "n = 1000")
}}
\keyword{dplot}
