\name{list.files}
\alias{list.files}
\title{List the Files in a Directory/Folder}
\usage{
list.files(path, pattern=NULL, all.files=FALSE,
    full.names=FALSE)
}
\arguments{
 \item{path}{the fill path name to the directory.}
 \item{pattern}{an optional regular expression.  Only file names
   which match the regular expression will be returned.}
 \item{all.files}{a logical value. If \code{FALSE}, only the
   names of visible files are returned.  If \code{TRUE},
   all file names will be returned.}
 \item{full.names}{a logical value.  If \code{TRUE}, the directory
   path is prepended to the file names.  If \code{FALSE}, only
   the file names are returned.}
}
\description{
This function produces a list containing the names of files
in the named directory.
}
\value{
A character vector containing the names of the files in the
specified directory, or \code{""} if there were no files.
}
\author{Ross Ihaka.}
\note{File naming conventions are very platform dependent.}

\examples{
list.files(R.home())
}
\keyword{file}
