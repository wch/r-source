\name{unlist}
\title{Flatten Lists}
\description{
  Given a list structure \code{x}, \code{unlist} simplifies it to
  produce a vector which contains all the atomic components
  which occur in \code{x}.
}
\usage{
unlist(x, recursive = TRUE, use.names = TRUE)
}
\alias{unlist}
\arguments{
  \item{x}{A list or vector.}
  \item{recursive}{logical. Should unlisting be applied to list
    components of \code{x}?}
  \item{use.names}{logical. Should names be preserved?}
}
\details{
  \code{unlist} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
  
  If \code{recursive = FALSE}, the function will not recurse beyond
  the first level items in \code{x}.

  \code{x} can be a vector, but then \code{unlist} does nothing useful,
  not even drop names.

  By default, \code{unlist} tries to retain the naming
  information present in \code{x}.
  If \code{use.names = FALSE} all naming information is dropped.

  Where possible the list elements are coerced to a common mode during
  the unlisting, and so the result often ends up as a character vector.

  A list is a (generic) vector, and the simplified vector might still be
  a list (and might be unchanged).  Non-vector elements of the list
  (for example language elements such as names, formulas and calls)
  are not coerced, and so a list containing one or more of these remains a
  list.  (The effect of unlisting an \code{\link{lm}} fit is a list which
  has individual residuals as components,)
}
\value{
  A vector of an appropriate mode to hold the list components.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{c}}, \code{\link{as.list}}.
}
\examples{
unlist(options())
unlist(options(), use.names=FALSE)

l.ex <- list(a = list(1:5, LETTERS[1:5]), b = "Z", c = NA)
unlist(l.ex, recursive = FALSE)
unlist(l.ex, recursive = TRUE)

l1 <- list(a="a", b=2, c=pi+2i)
unlist(l1) # a character vector
l2 <- list(a="a", b=as.name("b"), c=pi+2i)
unlist(l2) # remains a list
}
\keyword{list}
\keyword{manip}
