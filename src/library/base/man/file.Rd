\name{file}
\alias{file}
\title{File manipulation}
\usage{
file.create(file)
file.exists(file)
file.remove(file)
file.append(file1, file2)
}
\alias{file.create}
\alias{file.append}
\alias{file.remove}
\alias{file.exists}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{file, file1, file2}{character vectors, containing file names.}
}
\description{
These functions provide a very basic interface to the computer's
filestore.  

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
second argument to those named by its first.  The R subscript
recycling rule is used to align names given in vectors
of different lengths.
}
\author{
Ross Ihaka.
}
\seealso{\code{\link{file.show}}, \code{\link{list.files}}.}

\examples{
cat("file A\n", file="A")
cat("file B\n", file="B")
file.append("A", "B")
file.create("A")
file.append("A", rep("B", 10))
# file.show("A")
file.remove("A", "B")
}
\keyword{file}
