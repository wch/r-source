\name{vector}
\alias{vector}
\alias{as.vector}
\alias{as.vector.factor}
\alias{is.vector}
\title{Vectors}
\usage{
vector(mode = "logical", length = 0)
as.vector(x, mode = "any")
is.vector(x, mode = "any")
}
\arguments{
  \item{mode}{A character string giving an atomic mode, or \code{"any"}.}
  \item{length}{A non-negative integer specifying the desired length.}
  \item{x}{An object.}
}
\description{
  \code{vector} produces a vector of the given length and mode.

  \code{as.vector}, a generic, attempts to coerce its argument into a
  vector of mode \code{mode} (the default is to coerce to whichever mode
  is most convenient).  The attributes of \code{x} are removed.

  \code{is.vector} returns \code{TRUE} if \code{x} is a vector (of mode
  logical, integer, real, complex, character, raw or list if not specified)
  and \code{FALSE} otherwise.
}
\details{
  \code{is.vector} returns \code{FALSE} if \code{x} has any attributes
  except names.  (This is incompatible with S.)  On the other hand,
  \code{as.vector} removes \emph{all} attributes including names.
  
  Note that factors are \emph{not} vectors;  \code{is.vector} returns
  \code{FALSE} and \code{as.vector} converts to character mode.
}
\value{
  For \code{vector}, a vector of the given length and mode.  Logical
  vector elements are initialized to \code{FALSE}, numeric vector
  elements to \code{0}, character vector elements to \code{""}, raw
  vector elements to \code{nul} bytes and list elements to \code{NULL}.
}
\note{
  \code{as.vector} and \code{is.vector} are quite distinct from the
  meaning of the formal class \code{"vector"} in the \pkg{methods}
  package, and hence \code{\link{as}(x, "vector")} and
  \code{\link{is}(x, "vector")}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
\code{\link{c}}, \code{\link{is.numeric}}, \code{\link{is.list}}, etc.
}
\examples{
df <- data.frame(x=1:3, y=5:7)
\dontrun{## Error:
  as.vector(data.frame(x=1:3, y=5:7), mode="numeric")
}

x <- c(a = 1, b = 2)
is.vector(x)
as.vector(x)
all.equal(x, as.vector(x)) ## FALSE


###-- All the following are TRUE:
is.list(df)
! is.vector(df)
! is.vector(df, mode="list")

is.vector(list(), mode="list")
is.vector(NULL,   mode="NULL")
}
\keyword{classes}
