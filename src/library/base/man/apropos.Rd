\name{apropos}
\title{Find Objects by Partial Name}
\usage{apropos(pattern, where=FALSE)}
\arguments{
\item{pattern}{a regular expression to match against}
\item{where}{a logical indicating whether positions in the search list
should also be returned}
}
\alias{apropos}
\description{
\code{apropos} returns a vector of character strings giving the names of
all objects in the search list matching \code{pattern}.  If \code{where}
is \code{TRUE}, their position in the search list is returned as their
names attribute.
}
\author{Kurt Hornik and Martin Maechler (May 1997).}
\seealso{
\code{\link{objects}} for listing objects from one place,
\code{\link{search}} for the search path.
}
\examples{
apropos("lm")
apropos("ls")
apropos("lq")

# need a DOUBLE backslash `\\\\' (in case you don't see it anymore)
apropos("\\\\[")

# everything
length(apropos("."))

# those starting with `pr'
apropos("^pr")

# the 1-letter things
apropos("^.$")
# the 1-2-letter things
apropos("^..?$")
# the 2-to-4 letter things
apropos("^.{2,4}$")

# the 8-and-more letter things
apropos("^.{8,}$")
table(nchar(apropos("^.{8,}$")))
}
\keyword{data}
\keyword{documentation}
\keyword{environment}
