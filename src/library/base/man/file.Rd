\name{file}
\alias{file}
\title{File Manipulation}
\usage{
file.create(\dots)
file.exists(\dots)
file.remove(\dots)
file.append(file1, file2)
dir.create(path)
basename(path)
dirname(path)
}
\alias{file.create}
\alias{file.append}
\alias{file.remove}
\alias{file.exists}
\alias{dir.create}
\alias{basename}
\alias{dirname}
\arguments{
 \item{\dots, file1, file2, path}{character vectors, containing file names.}
}
\description{
These functions provide a very basic interface to the computer's
filestore.  
}
\details{
The \code{\dots} arguments are concatenated to form one character
string: you can specify the files separately or as one vector.

\code{file.create} creates files with the given names if they
do not already exist and truncates them if they do.
It returns a logical vector indicating the success or failure
of the operation for each file.

\code{file.exists} returns a logical vector indicating whether
the files named by its argument exist.

\code{file.remove} attempts to remove the files named in its
argument.  It returns a logical vector indicating whether or
not it succeeded in removing each file.

\code{file.append} attempts to append the files named by its
second argument to those named by its first.  The \R subscript
recycling rule is used to align names given in vectors
of different lengths.

\code{dir.create} creates the last element of the path. It returns a
logical, true for success.

\code{basename} removes all of the path up to the last path separator
(if any).

\code{dirname} returns the part of the \code{path} up to (but excluding)
the last path separator, or \code{"."} if there is no path separator.

In both \code{basename} and \code{dirname} trailing file separators
are removed before dissecting the path, and for \code{dirname} any
trailing file separators are removed from the result.
}
\author{
Ross Ihaka, Brian Ripley
}
\seealso{\code{\link{file.show}}, \code{\link{list.files}}.}

\examples{
cat("file A\n", file="A")
cat("file B\n", file="B")
file.append("A", "B")
file.create("A")
file.append("A", rep("B", 10))
if(interactive()) file.show("A")

file.remove("A","B")

basename(file.path("","p1","p2","p3","filename"))
dirname(file.path("","p1","p2","p3","filename"))
}
\keyword{file}
