\name{list.files}
\alias{list.files}
\alias{dir}
\title{List the Files in a Directory/Folder}
\usage{
list.files(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE)
       dir(path = ".", pattern = NULL, all.files = FALSE,
           full.names = FALSE, recursive = FALSE)
}
\arguments{
  \item{path}{a character vector of full path names; the default
    corresponds to the working directory \code{\link{getwd}()}.}
  \item{pattern}{an optional \link{regular expression}.  Only file names
    which match the regular expression will be returned.}
  \item{all.files}{a logical value. If \code{FALSE}, only the
    names of visible files are returned.  If \code{TRUE},
    all file names will be returned.}
  \item{full.names}{a logical value.  If \code{TRUE}, the directory
    path is prepended to the file names.  If \code{FALSE}, only
    the file names are returned.}
  \item{recursive}{logical. Should the listing recurse into directories?}
}
\description{
  This function produces a list containing the names of files
  in the named directory. \code{dir} is an alias.
}
\value{
  A character vector containing the names of the files in the
  specified directories, or \code{""} if there were no files. If a
  path does not exist or is not a directory or is unreadable it
  is skipped, with a warning.

  The files are sorted in alphabetical order, on the full path
  if \code{full.names = TRUE}.
}
\author{Ross Ihaka, Brian Ripley}
\note{
  File naming conventions are very platform dependent.

  \code{recursive = TRUE} is not supported on all platforms, and may be
  ignored, with a warning.
}
\seealso{\code{\link{file.info}}, \code{\link{file.access}}
  and \code{\link{files}} for many more file handling functions and 
  \code{\link{file.choose}} 
#ifdef windows
and \code{\link{choose.files}}
#endif
for interactive selection.}
\examples{
list.files(R.home())
## Only files starting with a-l or r (*including* uppercase):
dir("../..", pattern = "^[a-lr]",full.names=TRUE)
}
\keyword{file}
