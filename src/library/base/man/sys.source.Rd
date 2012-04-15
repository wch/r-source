% File src/library/base/man/sys.source.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{sys.source}
\alias{sys.source}
\title{Parse and Evaluate Expressions from a File}
\description{
  Parses expressions in the given file, and then successively evaluates
  them in the specified environment.
}
\usage{
sys.source(file, envir = baseenv(), chdir = FALSE,
           keep.source = getOption("keep.source.pkgs"))
}
\arguments{
  \item{file}{a character string naming the file to be read from}
  \item{envir}{an \R object specifying the environment in which the
    expressions are to be evaluated.  May also be a list or an integer.
    The default value \code{NULL} corresponds to evaluation in the base
    environment.  This is probably not what you want; you should
    typically supply an explicit \code{envir} argument.}
  \item{chdir}{logical; if \code{TRUE}, the \R working directory is
    changed to the directory containing \code{file} for evaluating.}
  \item{keep.source}{logical.  If \code{TRUE}, functions keep
    their source including comments, see
    \code{\link{options}(keep.source = *)} for more details.}
}
\details{
  For large files, \code{keep.source = FALSE} may save quite a bit of
  memory.
%%FIXME:  the following paragraph should be somewhere it's likely to be seen
  In order for the code being evaluated to use the correct environment
  (for example, in global assignments), source code in packages should
  call \code{\link{topenv}()}, which will return the namespace, if any,
  the environment set up by \code{sys.source}, or the global environment
  if a saved image is being used.
}
\seealso{\code{\link{source}}, and \code{\link{library}} which uses
  \code{sys.source}.
}
\examples{
## a simple way to put some objects in an environment
## high on the search path
tmp <- tempfile()
writeLines("aaa <- pi", tmp)
env <- attach(NULL, name = "myenv")
sys.source(tmp, env)
unlink(tmp)
search()
aaa
detach("myenv")
}
\keyword{file}
\keyword{utilities}
