\name{ls}
\title{List Objects}
\usage{
ls(name, pos= -1, envir=pos.to.env(pos),
       all.names=FALSE, pattern)
objects(name, pos= -1, envir=pos.to.env(pos),
       all.names=FALSE, pattern)
}
\alias{ls}
\alias{objects}
\arguments{
\item{name}{the name of an attached object appearing
in the vector of names returned by \code{search}.}
\item{pos}{the index of an attached object in
  the list returned by \code{search}.  Defaults to the \emph{current}
  environment.}
\item{envir}{an evaluation environment.  Defaults to the one corresponding
  to \code{pos}.}
\item{all.names}{a logical value.  If \code{TRUE}, all
  object names are returned.  If \code{FALSE}, names which begin with a
  ``.'' are omitted.}
\item{pattern}{an optional regular expression, see \code{\link{grep}}.
  Only names matching \code{pattern} are returned.}
}
\description{
\code{ls} and \code{objects} return a vector of character strings
giving the names of the objects in the specified environment.
When invoked with no argument at the top level prompt,
\code{ls} shows what data sets and functions a user has defined.
When invoked with no argument inside a function,
\code{ls} returns the names of the functions local variables.
This is useful in conjunction with \code{browser}.
}
\note{
    It is possible to compile \R so that \code{grep} and hence argument
    \code{pattern} and not operational.
}
\seealso{
\code{\link{apropos}} (or \code{\link{find}}) for finding objects in the
whole search path; \code{\link{grep}} for more details on ``regular
expressions''; \code{\link{class}}, \code{\link{methods}}, etc. for
object-oriented programming.
}
\examples{
.Ob <- 1
ls(pat="O")
ls(pat="O", all = T)    # also shows ".[foo]"

# shows an empty list because inside myfunc no variables are defined
myfunc _ function() {ls()}
myfunc()

# define a local variable inside myfunc
myfunc _ function() {y_1; ls()}
myfunc()                # shows "y"
}
\keyword{environment}
