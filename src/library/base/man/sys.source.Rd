\name{sys.source}
\alias{sys.source}
\title{Parse and Evaluate Expressions from a File}
\description{
  Parses expressions in the given file, and then successively evaluates
  them in the specified environment.  By default, evaluation is done in
  the global environment.
}
\usage{sys.source(file, envir = NULL)}
\arguments{
  \item{file}{a character string naming the file to be read from}
  \item{envir}{an \R object specifying the environment in which the
    expressions are to be evaluated.  May also be a list or an integer.}
}
\details{
  The \code{keep.source} option is set to \code{FALSE} when reading from
  the file, so the source code is not kept (see \code{\link{options}}
  for more details).
}
\seealso{
  \code{\link{source}}
}
\keyword{file}
\keyword{utilities}
