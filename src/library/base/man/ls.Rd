\name{ls}
\alias{ls}
\alias{objects}
\title{List Objects}
\usage{
ls(name, pos = -1, envir = as.environment(pos),
   all.names = FALSE, pattern)
objects(name, pos= -1, envir = as.environment(pos),
        all.names = FALSE, pattern)
}
\arguments{
  \item{name}{which environment to use in listing the available objects.
    Defaults to the \emph{current}
    environment.  Although called \code{name} for back compatibility, in
    fact this argument can specify the environment in any form; see the
    details section.}
  \item{pos}{An alternative  argument to \code{name} for specifying the
    environment as a position in the search list.  Mostly there for
    back compatibility.}
  \item{envir}{an alternative  argument to \code{name} for specifying the
    environment evaluation environment. Mostly there for back compatibility.}
  \item{all.names}{a logical value.  If \code{TRUE}, all
    object names are returned.  If \code{FALSE}, names which begin with a
    \samp{.} are omitted.}
  \item{pattern}{an optional \link{regular expression}. Only names
    matching \code{pattern} are returned.}
}
\details{
  The \code{name} argument can specify the  environment from which
  object names are taken in one of several forms:
  as an integer (the position in the \code{\link{search}} list); as
  the character string name of an element in the search list; or as an
  explicit \code{\link{environment}} (including using
  \code{\link{sys.frame}} to access the currently active function calls).
  By default, the environment of the call to \code{ls} or \code{objects}
  is used. The \code{pos} and \code{envir} arguments are an alternative
  way to specify an environment, but are primarily there for back
  compatibility.
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{ls.str}} for a long listing based on \code{\link{str}}.
  \code{\link{apropos}} (or \code{\link{find}})
  for finding objects in the whole search path;
  \code{\link{grep}} for more details on \dQuote{regular expressions};
  \code{\link[base]{class}},
  \code{\link{methods}}, etc., for
  object-oriented programming.
}
\examples{
.Ob <- 1
ls(pat="O")
ls(pat="O", all = TRUE)    # also shows ".[foo]"

# shows an empty list because inside myfunc no variables are defined
myfunc <- function() {ls()}
myfunc()

# define a local variable inside myfunc
myfunc <- function() {y <- 1; ls()}
myfunc()                # shows "y"
}
\keyword{environment}
