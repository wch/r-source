\name{Colon}
\title{Colon Operator}
\alias{:}
\description{
  Generate regular sequences.
}
\usage{
from:to
   a:b
}
\arguments{
  \item{from}{starting value of sequence.}
  \item{to}{(maximal) end value of the sequence.}
  \item{a, b}{\code{\link{factor}}s of same length.}
}
\details{
  The binary operator \code{:} has two meanings: for factors \code{a:b} is
  equivalent to \code{\link{interaction}(a, b)} (except for labelling by
  \code{la:lb} not \code{la.lb}).


  For numeric arguments \code{from:to} is equivalent to \code{seq(from, to)},
  and generates a sequence from \code{from} to \code{to} in steps of \code{1}
  or \code{1-}.  Value \code{to} will be included if it differs from
  \code{from} by an integer up to a numeric fuzz of about \code{1e-7}.
}
\value{
  For numeric arguments, a numeric vector.  This will be of type
  \code{\link{integer}} if \code{from} and \code{to} are both integers
  and representable in the integer type, otherwise of type
  \code{\link{numeric}}.

  For factors, a factor.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{seq}}.
  
  As an alternative to using \code{:} for factors, \code{\link{interaction}}.
  
  For \code{:} used in the formal representation of an interaction, see 
  \code{\link{formula}}.
}
\examples{
1:4
pi:6 # real
6:pi # integer

f1 <- gl(2,3); f1
f2 <- gl(3,2); f2
f1:f2 # a factor, the "cross"  f1 x f2
}
\keyword{manip}
