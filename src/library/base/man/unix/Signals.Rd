\name{Signals}
\title{Interrupting Execution of R}
\alias{Signals}
\usage{
kill -USR1 pid
kill -USR2 pid
}
\description{On receiving \code{SIGUSR1} \R will save the workspace and
  quit. \code{SIGUSR2} has the same result except that the
  \code{\link{.Last}} function and \code{\link{on.exit}} expressions
  will not be called.}

\arguments{
  \item{pid}{The process ID of the R process}
}

\section{Warning}{It is possible that one or more R objects will be
  undergoing modification at the time the signal is sent. These objects
  could be saved in a corrupted form.}
\keyword{utilities}
    
