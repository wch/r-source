\name{missing}
\alias{missing}
\title{Does a Formal Argument have a Value?}
\usage{
missing(x)
}
\description{
\code{missing} can be used to test whether a value was specified
as an argument to a function.
}
\arguments{
  \item{x}{a formal argument.}
}
\details{
  \code{missing(x)} is only reliable if \code{x} has not been altered
  since entering the function: in particular it will \emph{always}
  be false after \code{x <- match.arg(x)}. 
  
  The example shows how a plotting function
  can be written to work with either a pair of vectors
  giving x and y coordinates of points to be plotted
  or a single vector giving y values to be plotted
  against their indexes.

  Currently \code{missing} can only be used in the immediate body of
  the function that defines the argument, not in the body of a nested
  function or a \code{local} call.  This may change in the future.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language}.
  Springer.
}
\seealso{
  \code{\link{substitute}} for argument expression;
  \code{\link{NA}} for \dQuote{missing values} in data.
}
\examples{
myplot <- function(x,y) {
                if(missing(y)) {
                        y <- x
                        x <- 1:length(y)
                }
                plot(x,y)
        }
}
\keyword{programming}
