\name{files}
\alias{files}
\title{File Manipulation}
\usage{
file.create(\dots)
file.exists(\dots)
file.remove(\dots)
file.append(file1, file2)
file.copy(from, to, overwrite = FALSE)
dir.create(path)
basename(path)
dirname(path)
path.expand(path)
}
\alias{file.append}
\alias{file.copy}
\alias{file.create}
\alias{file.exists}
\alias{file.remove}
\alias{dir.create}
\alias{basename}
\alias{dirname}
\alias{path.expand}
\arguments{
  \item{\dots, file1, file2, from, to, path}{character vectors,
    containing file names.}
  \item{overwrite}{logical; should the destination files be overwritten?}
}
\description{
  These functions provide a low-level interface to the computer's
  file system.
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

  \code{file.copy} works in a similar way to \code{file.append} but with
  the arguments in the natural order for copying. Copying to existing
  destination files is skipped unless \code{overwrite = TRUE}.
  The \code{to} argument can specify a single existing directory.

  \code{dir.create} creates the last element of the path. It returns a
  logical, true for success.

  \code{basename} removes all of the path up to the last path separator
  (if any).

  \code{dirname} returns the part of the \code{path} up to (but
  excluding) the last path separator, or \code{"."} if there is no path
  separator.  Tilde expansion is done: see the description for
  \code{path.expand} below.

  In both \code{basename} and \code{dirname} trailing file separators
  are removed before dissecting the path, and for \code{dirname} any
  trailing file separators are removed from the result.

  \code{path.expand} expands path(s) by replacing a leading tilde by the
  user's home directory (if defined on that platform).
#ifdef unix
  On \emph{some Unix} versions, a leading \code{~user} will expand to
  the home directory of \code{user}, but not on Unix versions without
  \code{readline} installed.
#endif
}
\author{
  Ross Ihaka, Brian Ripley
}
\seealso{
  \code{\link{file.info}}, \code{\link{file.access}}, \code{\link{file.path}},
  \code{\link{file.show}}, \code{\link{list.files}},
  \code{\link{unlink}}.
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
unlink("tmp", recursive=TRUE)
file.remove("A", "B", "C")

basename(file.path("","p1","p2","p3","filename"))
dirname(file.path("","p1","p2","p3","filename"))

path.expand("~/foo")
}
\keyword{file}
