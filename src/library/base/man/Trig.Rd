\name{Trig}
\alias{Trig}
\alias{cos}
\alias{sin}
\alias{tan}
\alias{acos}
\alias{asin}
\alias{atan}
\alias{atan2}
\title{Trigonometric Functions}
\description{
  These functions give the obvious trigonometric functions.  They
  respectively compute the cosine, sine, tangent, arc-cosine, arc-sine,
  arc-tangent, and the two-argument arc-tangent.
}
\usage{
cos(x)
sin(x)
tan(x)
acos(x)
asin(x)
atan(x)
atan2(y, x)
}
\arguments{
  \item{x, y}{numeric vector}
}
\details{
  The arc-tangent of two arguments \code{atan2(y,x)} returns the angle
  between the x-axis and the vector from the origin to \eqn{(x,y)}, i.e., for
  positive arguments \code{atan2(y,x) == atan(y/x)}.

  Angles are in radians, not degrees (i.e. a right angle is \eqn{\pi/2}).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\keyword{math}
