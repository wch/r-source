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
\examples{
(WD <- getwd())
if (!is.null(WD)) setwd(WD)
}
\keyword{utilities}
