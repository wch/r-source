\name{tkfilefind}
\alias{tkfilefind}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{File selection browser}
\description{
\code{tkfilefind} produces a directory tree browser allowing one or more
files to be selected and the file names returned to \R.
}
\usage{
tkfilefind(path=getwd(), all.names=FALSE, multiple=FALSE)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{path}{Directory to start browsing}
  \item{all.names}{Show hidden files under Unix?}
  \item{multiple}{Allow multiple file selection?}
}

\value{
A vector of absolute filenames
}
\references{Uses the tree widget from \url{http://www.hobbs.wservice.com/tcl/script/widget} }


\examples{
\dontrun{tkfilefind()}
}
\keyword{misc}%-- one or more ...
