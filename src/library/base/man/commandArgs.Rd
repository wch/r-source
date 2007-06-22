\name{commandArgs}
\alias{commandArgs}
\title{Extract Command Line Arguments}
\description{
  Provides access to a copy of the command line arguments supplied when
  this \R session was invoked.
}
\usage{
commandArgs(trailingOnly=FALSE)
}
\arguments{
  \item{trailingOnly}{logical.  Should only arguments after
    \code{--args} be returned?}
}
\details{
  These arguments are captured before the standard \R command line
  processing takes place.  This means that they are the unmodified
  values.  If it were useful, we could provide support an argument which
  indicated whether we want the unprocessed or processed values.

  This is especially useful with the \code{--args} command-line flag to
  \R, as all of the command line after that flag is skipped.
}
\value{
  A character vector containing the name of the executable and the
  user-supplied command line arguments.  The first element is the name
  of the executable by which \R was invoked.  As far as I am aware, the
  exact form of this element is platform dependent.  It may be the fully
  qualified name, or simply the last component (or basename) of the
  application.

  If \code{trailingOnly=TRUE}, a character vector of those arguments
  (if any) supplied after \code{--args}.
}
%\author{DTL}
\seealso{\code{\link{Startup}}
#ifdef unix
\code{\link{BATCH}}
#endif
}
\examples{
commandArgs()
## Spawn a copy of this application as it was invoked.
## system(paste(commandArgs(), collapse=" "))
}
\keyword{environment}
\keyword{sysdata}
\keyword{programming}
