\name{RNG}
\title{Random Number Generators}
\alias{RNG}
\alias{RNGkind}
\usage{
RNGkind(kind=NULL)
}
\description{
Query or set the kind of Random Number Generator in Use.
}
\arguments{
    \item{kind}{character or \code{NULL}.  If \code{kind} is a character
	string, set \R's RNG to the kind desired, if it's \code{NULL},
	return the currently used RNG.
}
\value{the RNG in use (\emph{before} the call).
}
\details{

    
    --- --- to be written --- ---

    
	Available kinds:

	\itemize{
	    \item "Wichmann-Hill"
	    \item "Marsaglia-Multicarry"
	    \item "Super-Duper"
	    \item "Mersenne-Twister"
	    \item "Rand"
	    }

}
\references{
}
\seealso{\code{\link{.Random.seed}}, \code{\link{runif}}, \ldots.
}
\examples{
ok <- RNGkind()
RNGkind("Super")#matches  "Super-Duper"
RNGkind()

## Reset:
RNGkind(ok)
}
\author{Martin Maechler}
\keyword{distribution}
