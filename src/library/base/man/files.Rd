\name{files}
\alias{files}
\concept{directory}
\title{File and Directory Manipulation}
\usage{
file.create(\dots)
file.exists(\dots)
file.remove(\dots)
file.rename(from, to)
file.append(file1, file2)
file.copy(from, to, overwrite = FALSE)
file.symlink(from, to)
dir.create(path, showWarnings = TRUE)
}
\alias{file.append}
\alias{file.copy}
\alias{file.create}
\alias{file.exists}
\alias{file.remove}
\alias{file.rename}
\alias{file.symlink}
\alias{dir.create}
\arguments{
  \item{\dots, file1, file2, from, to}{character vectors,
    containing file names.}
  \item{path}{a character vector containing a single path name.}
  \item{overwrite}{logical; should the destination files be overwritten?}
  \item{showWarnings}{logical; should the warnings on failure be shown?}
}
\description{
  These functions provide a low-level interface to the computer's
  file system.
}
\details{
  The \code{\dots} arguments are concatenated to form one character
  string: you can specify the files separately or as one vector.
  All of these functions expand path names: see \code{\link{path.expand}}.

  \code{file.create} creates files with the given names if they
  do not already exist and truncates them if they do.

  \code{file.exists} returns a logical vector indicating whether
  the files named by its argument exist.

  \code{file.remove} attempts to remove the files named in its
  argument.

  \code{file.rename} attempts to rename a single file.
#ifdef windows
  On Windows 9x/ME rename is not atomic, so it is possible that
  \code{to} will be deleted but \code{from} will not be renamed.
#endif

  \code{file.append} attempts to append the files named by its
  second argument to those named by its first.  The \R subscript
  recycling rule is used to align names given in vectors
  of different lengths.

  \code{file.copy} works in a similar way to \code{file.append} but with
  the arguments in the natural order for copying. Copying to existing
  destination files is skipped unless \code{overwrite = TRUE}.
  The \code{to} argument can specify a single existing directory.

  \code{file.symlink} makes symbolic links on those Unix-like platforms
  which support them.  The \code{to} argument can specify a single
  existing directory.
  
  \code{dir.create} creates the last element of the path.
}
\value{
  \code{dir.create} and \code{file.rename} return a logical,
  true for success.

  The remaining functions return a logical vector indicating which
  operation succeeded for each of the files attempted.

  \code{dir.create} will return failure if the directory already exists. 
}
\author{
  Ross Ihaka, Brian Ripley
}
\seealso{
  \code{\link{file.info}}, \code{\link{file.access}}, \code{\link{file.path}},
  \code{\link{file.show}}, \code{\link{list.files}},
  \code{\link{unlink}}, \code{\link{basename}}, \code{\link{path.expand}}.
}

\examples{
cat("file A\n", file="A")
cat("file B\n", file="B")
file.append("A", "B")
file.create("A")
file.append("A", rep("B", 10))
if(interactive()) file.show("A")
file.copy("A", "C")
dir.create("tmp")
file.copy(c("A", "B"), "tmp")
list.files("tmp")
#ifdef unix
setwd("tmp")
file.remove("B")
file.symlink(file.path("..", c("A", "B")), ".")
setwd("..")
#endif
unlink("tmp", recursive=TRUE)
file.remove("A", "B", "C")
}
\keyword{file}
