\name{dump}
\title{Text Representations of \R Objects}
\usage{
dump(list, fileout="dumpdata")
}
\alias{dump}
\value{
This function takes a list names of \R objects and produces
text representations of the objects in the
given file.
At present the implementation of \code{dump} is very
incomplete and it really only works for functions and
simple vectors.

The function save is designed to be used for
transporting \R data between machines.
}
\seealso{
\code{\link{dput}}, \code{\link{dget}},
\code{\link{write}}.
}
\examples{
dump(ls(patt='^[xyz]'), "xyz.Rdmped")
}
\keyword{file}
