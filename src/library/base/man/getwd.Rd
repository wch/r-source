\name{getwd}
\alias{getwd}
\alias{setwd}
\title{Get or Set Working Directory}
\usage{
getwd()
setwd(dir)
}
\arguments{
  \item{dir}{A character string.}
}
\description{
  \code{getwd} returns an absolute filename representing the current
  working directory of the \R process; \code{setwd(dir)} is used to set
  the working directory to \code{dir}.
}
\note{
  These functions are not implemented on all platforms.
}
\seealso{\code{\link{list.files}} for the \emph{contents} of a directory.
}
\value{
  \code{getwd} returns a character vector, or \code{NULL} if the
  working directory is not available on that platform.

  \code{setwd} returns \code{NULL} invisibly.  It will give an error if
  it does not succeed.
}
\examples{
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
}
\keyword{utilities}
