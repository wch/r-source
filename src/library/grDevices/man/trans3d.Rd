% File src/library/grDevices/man/trans3d.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{trans3d}
\alias{trans3d}
\title{3D to 2D Transformation for Perspective Plots}
\description{
  Projection of 3-dimensional to 2-dimensional points using a 4x4
  viewing transformation matrix.  Mainly for adding to
  perspective plots such as \code{\link{persp}}.
}
\usage{
trans3d(x, y, z, pmat)
}
\arguments{
  \item{x, y, z}{numeric vectors of equal length, specifying points in
    3D space.}
  \item{pmat}{a \eqn{4 \times 4}{4 x 4} \emph{viewing transformation matrix},
    suitable for projecting the 3D coordinates \eqn{(x,y,z)} into the 2D
    plane using homogeneous 4D coordinates \eqn{(x,y,z,t)};
    such matrices are returned by \code{\link{persp}()}.}
}
\value{
  a list with two components
  \item{x,y}{the projected 2d coordinates of the 3d input \code{(x,y,z)}.}
}
\seealso{ \code{\link{persp}} }
\examples{
%% it would be nice to have an independent example
## See  help(persp) {after attaching the 'graphics' package}
##      -----------
}
\keyword{dplot}
