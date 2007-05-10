\name{Math}
\title{Miscellaneous Mathematical Functions}
\usage{
abs(x)
sqrt(x)
}
\alias{abs}
\alias{sqrt}
\description{
  These functions compute miscellaneous mathematical functions.
  The naming follows the standard for computer languages
  such as C or Fortran.
}
\arguments{
  \item{x}{a numeric or \code{\link{complex}} vector or array.}
}
\details{
  These are generic functions: methods can be defined for them
  individually or via the \code{\link[base:groupGeneric]{Math}}
  group generic.  For complex arguments (and the default method), \code{z},
  \code{abs(z) == \link{Mod}(z)} and \code{sqrt(z) == z^0.5}.

  \code{abs(x)} returns an \code{\link{integer}} vector when \code{x} is
  \code{integer} or \code{\link{logical}}.
}
\section{S4 methods}{
  Both are S4 generic and members of the
  \code{\link[methods:S4groupGeneric]{Math}} group generic.
}
\seealso{
  \code{\link{Arithmetic}} for simple, \code{\link{log}} for logarithmic,
  \code{\link{sin}} for trigonometric, and \code{\link{Special}} for
  special mathematical functions.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(stats) # for spline
require(graphics)
xx <- -9:9
plot(xx, sqrt(abs(xx)),  col = "red")
lines(spline(xx, sqrt(abs(xx)), n=101), col = "pink")
}
\keyword{math}
