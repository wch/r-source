\name{sys.source}
\alias{sys.source}
\title{Parse and Evaluate Expressions from a File}
\description{
  Parses expressions in the given file, and then successively evaluates
  them in the specified environment.  By default, evaluation is done in
  the user's workspace (the global environment).
}
\usage{
sys.source(file, envir = NULL, chdir = FALSE,
           keep.source = getOption("keep.source.pkgs"))
}
\arguments{
  \item{file}{a character string naming the file to be read from}
  \item{envir}{an \R object specifying the environment in which the
    expressions are to be evaluated.  May also be a list or an integer.}
  \item{chdir}{logical; if \code{TRUE}, the \R working directory is
    changed to the directory containing \code{file} for evaluating.}
  \item{keep.source}{logical.  If \code{TRUE}, functions ``keep their
    source'' including comments, see \code{\link{options}(keep.source = *)}
    for more details.}
}
\details{
  For large files, \code{keep.source = FALSE} may save quite a bit of
  memory.
}
\seealso{\code{\link{source}}, and \code{\link{library}} which uses
  \code{sys.source}.
}
\keyword{file}
\keyword{utilities}
