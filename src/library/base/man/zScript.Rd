\name{.Script}
\title{Scripting Language Interface}
\alias{.Script}
\description{
  Run a script through its interpreter with given arguments.
}
\usage{
.Script(interpreter, script, args, \dots)
}
\arguments{
  \item{interpreter}{a character string naming the interpreter for the
    script.}
  \item{script}{a character string with the base file name of the
    script, which must be located in the \file{\code{interpreter}}
    subdirectory of \file{R\_HOME/share}.}
  \item{args}{a character string giving the arguments to pass to the
    script.}
  \item{\dots}{further arguments to be passed to \code{\link{system}}
    when invoking the interpreter on the script.}
}
\note{This function is for R internal use only.}
\examples{
.Script("perl", "maketitle.pl", file.path(.Library, "base", "TITLE"))
}
\keyword{interface}
