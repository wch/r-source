\name{list.files}
\alias{list.files}
\alias{dir}
\title{List the Files in a Directory/Folder}
\usage{
list.files(path, pattern=NULL, all.files=FALSE,
    full.names=FALSE)
dir(path, pattern=NULL, all.files=FALSE,
    full.names=FALSE)
}
\arguments{
 \item{path}{a vector of full path names.}
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
in the named directory. \code{dir} is an alias.
}
\value{
A character vector containing the names of the files in the
specified directory, or \code{""} if there were no files. The files are
sorted in alphabetical order, on the full path if \item{full.names = TRUE}.
}
\author{Ross Ihaka}
\note{
    File naming conventions are very platform dependent.

    The \code{pattern} argument will be ignored (with a warning) when \R
    is compiled without a regular expressions library.
}

\examples{
list.files(R.home())
}
\keyword{file}
