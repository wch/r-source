\name{quit}
\title{Terminate an \R Session}
\usage{
quit(save="ask")
q(save="ask")
}
\alias{quit}
\alias{q}
\arguments{
\item{save}{a character string indicating whether the 
environment (workspace) should be saved.}
}
\description{
\code{save} must be one of \code{"no"}, \code{"yes"},
or \code{"ask"}.
In the first case the workspace is not saved, in the second
it is saved and in the third the user is prompted.
Invoking these functions results in the termination
of the current \R session.
}
\keyword{environment}
