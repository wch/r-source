\name{data.class}
\alias{data.class}
\title{Object Classes}
\description{
  Determine the class of an arbitrary \R object.
}
\usage{
data.class(x)
}
\arguments{
  \item{x}{an \R object.}
}
\value{
  character string giving the ``class'' of \code{x}.

  The ``class'' is the (first element) of the \code{\link[base]{class}}
  attribute if this is non-\code{NULL}, or inferred from the object's
  \code{dim} attribute if this is non-\code{NULL}, or \code{mode(x)}.

  Simply speaking, \code{data.class(x)} returns what is typically useful
  for method dispatching.  (Or, what the basic creator functions already
  and maybe eventually all will attach as a class attribute.)
}
\seealso{
  \code{\link[base]{class}}
}
\examples{
x <- LETTERS
data.class(factor(x))                 # has a class attribute
data.class(matrix(x, nc = 13))        # has a dim attribute
data.class(list(x))                   # the same as mode(x)
data.class(x)                         # the same as mode(x)
}
\keyword{classes}
\keyword{methods}
