\name{Signals}
\alias{Signals}
\title{Interrupting Execution of R}
\description{
  On receiving \code{SIGUSR1} \R will save the workspace and quit.
  \code{SIGUSR2} has the same result except that the \code{\link{.Last}}
  function and \code{\link{on.exit}} expressions will not be called.
}
\usage{
\special{kill -USR1 pid}
\special{kill -USR2 pid}
}
\arguments{
  \item{pid}{The process ID of the R process}
}
\section{Warning}{
  It is possible that one or more R objects will be undergoing
  modification at the time the signal is sent.  These objects could be
  saved in a corrupted form.
}
\keyword{utilities}
