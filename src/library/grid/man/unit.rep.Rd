\name{unit.rep}
\alias{unit.rep}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Replicate Elements of Unit Objects }
\description{
  Replicates the units according to the values given
     in `times' and `length.out'.
}
\usage{
unit.rep(x, times, length.out)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{x}{ An object of class \code{"unit"}. }
  \item{times}{ integer.  A vector giving the number of times to repeat each
          element. Either of length 1 or `length(x)'. }
  \item{length.out}{ integer.  (Optional.)  The desired length of the output
          vector. }
}
\value{
  An object of class \code{"unit"}.
}
\author{ Paul Murrell }
 
 \seealso{
   \code{\link{rep}}
 }

\examples{
unit.rep(unit(1:3, "npc"), 3)
unit.rep(unit(1:3, "npc"), 1:3)
unit.rep(unit(1:3, "npc") + unit(1, "inches"), 3)
unit.rep(max(unit(1:3, "npc") + unit(1, "inches")), 3)
unit.rep(max(unit(1:3, "npc") + unit(1, "strwidth", "a"))*4, 3)
unit.rep(unit(1:3, "npc") + unit(1, "strwidth", "a")*4, 3)
}
\keyword{ dplot }% at least one, from doc/KEYWORDS
