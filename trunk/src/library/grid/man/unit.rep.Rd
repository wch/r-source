\name{unit.rep}
\alias{unit.rep}
\title{ Replicate Elements of Unit Objects }
\description{
  Replicates the units according to the values given in \code{times} and
  \code{length.out}.

  This function has been deprecated in favour of a unit method for
  the generic \code{rep} function.
}
\usage{
unit.rep(x, times, length.out)
}
\arguments{
  \item{x}{ An object of class \code{"unit"}. }
  \item{times}{integer.  A vector giving the number of times to repeat
    each element.  Either of length 1 or \code{length(x)}. }
  \item{length.out}{integer.  (Optional.)  The desired length of the
    output vector.}
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }
 
\seealso{
  \code{\link{rep}}
}

\examples{
rep(unit(1:3, "npc"), 3)
rep(unit(1:3, "npc"), 1:3)
rep(unit(1:3, "npc") + unit(1, "inches"), 3)
rep(max(unit(1:3, "npc") + unit(1, "inches")), 3)
rep(max(unit(1:3, "npc") + unit(1, "strwidth", "a"))*4, 3)
rep(unit(1:3, "npc") + unit(1, "strwidth", "a")*4, 3)
}
\keyword{dplot}
